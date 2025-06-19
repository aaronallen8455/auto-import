{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
module AutoImport
  ( plugin
  ) where

import           Control.Applicative ((<|>))
import           Control.Exception (try, throw)
import           Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Char as Char
import           Data.Containers.ListUtils (nubOrd)
import           Data.Either (partitionEithers)
import           Data.Maybe
import           Data.Foldable
import           Data.IORef
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import           Data.Time (UTCTime)
import           Data.Void
import           GHC.Generics (Generic, Generically(..))
import qualified GHC.Paths as Paths
import qualified Language.Haskell.GHC.ExactPrint as EP
import qualified Language.Haskell.GHC.ExactPrint.Parsers as EP
import qualified Language.Haskell.GHC.ExactPrint.Utils as EP
import qualified System.Directory as Dir
import           System.IO.Unsafe (unsafePerformIO)
import qualified Text.Megaparsec as Parse
import qualified Text.Megaparsec.Char as Parse

import qualified AutoImport.GhcFacade as Ghc

--------------------------------------------------------------------------------
-- Plugin
--------------------------------------------------------------------------------

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.driverPlugin = \_ hscEnv -> pure $ addHscHook hscEnv
  , Ghc.pluginRecompile = Ghc.purePlugin
  }

addHscHook :: Ghc.HscEnv -> Ghc.HscEnv
addHscHook hscEnv = hscEnv
  { Ghc.hsc_hooks =
      let hooks = Ghc.hsc_hooks hscEnv
       in hooks
          { Ghc.runPhaseHook = Just $ phaseHook (Ghc.runPhaseHook hooks) }
  }
  where
    phaseHook mExistingHook = Ghc.PhaseHook $ \phase -> case phase of
      Ghc.T_Hsc env modSum -> do
        let modFile = Ghc.ms_hspp_file modSum
            importsAddedErr =
              Ghc.mkPlainErrorMsgEnvelope
                (Ghc.mkGeneralSrcSpan $ Ghc.mkFastString modFile)
                (Ghc.ghcUnknownMessage ImportsAddedDiag)
        eTcRes <- try $ runPhaseOrExistingHook phase
        autoImportCfg <- resolveConfig
        let msgs = case eTcRes of
                     Left (Ghc.SourceError m) -> m
                     Right (_, m) -> m
            mkMissingThingError msgEnv =
              case Ghc.errMsgDiagnostic msgEnv of
                Ghc.GhcTcRnMessage
                    (Ghc.TcRnMessageWithInfo _
                      (Ghc.TcRnMessageDetailed _
                        (Ghc.TcRnNotInScope Ghc.NotInScope _ [Ghc.MissingModule missingMod] _))
                    )
                  | let modTxt = moduleNameToText missingMod
                  , Just qualMod <- M.lookup modTxt (qualModules autoImportCfg)
                  -> Right $ MissingModule qualMod
                Ghc.GhcTcRnMessage
                    (Ghc.TcRnMessageWithInfo _
                      (Ghc.TcRnMessageDetailed _
                        (Ghc.TcRnSolverReport report _)
                    ))
                  | Ghc.ReportHoleError hole (Ghc.OutOfScopeHole _ _)
                      <- Ghc.reportContent report
                  , Just unqualId <- M.lookup (T.pack . EP.rdrName2String $ Ghc.hole_occ hole) (unqualIdentifiers autoImportCfg)
                  -> Right $ MissingId unqualId
                _ -> Left msgEnv
            (_otherDiags, missingThings) =
              Ghc.partitionBagWith mkMissingThingError (Ghc.getMessages msgs)
        case NE.nonEmpty (nubOrd $ toList missingThings) of
          Just neMissing -> do
            -- Parse from file because the parse result from GHC lacks comments
            let dynFlags = Ghc.ms_hspp_opts modSum `Ghc.gopt_set` Ghc.Opt_KeepRawTokenStream
            (eParseResult, usesCpp) <- parseModule env dynFlags modFile
            case eParseResult of
              Left errs -> throw $ Ghc.mkSrcErr errs
              Right parseResult -> do
                modifyModule parseResult usesCpp neMissing modFile
                throw . Ghc.mkSrcErr . Ghc.mkMessages $ Ghc.unitBag importsAddedErr
          _ -> either throw pure eTcRes
      _ -> runPhaseOrExistingHook phase
      where
      runPhaseOrExistingHook :: Ghc.TPhase res -> IO res
      runPhaseOrExistingHook = maybe Ghc.runPhase (\(Ghc.PhaseHook h) -> h) mExistingHook

moduleNameToText :: Ghc.ModuleName -> T.Text
moduleNameToText = TE.decodeUtf8 . Ghc.bytesFS . Ghc.moduleNameFS

data MissingThing
  = MissingModule QualMod
  | MissingId UnqualIdentifier
  deriving (Eq, Ord)

-- | Diagnostic thrown when import statements are inserted
data ImportsAddedDiag = ImportsAddedDiag

instance Ghc.Diagnostic ImportsAddedDiag where
  type DiagnosticOpts ImportsAddedDiag = Ghc.NoDiagnosticOpts
  diagnosticMessage _ _ = Ghc.mkSimpleDecorated $
    Ghc.text "Module updated by auto-import, compilation aborted"
  diagnosticReason _ = Ghc.ErrorWithoutFlag
  diagnosticHints _ = []
  diagnosticCode _ = Nothing
#if !MIN_VERSION_ghc(9,8,0)
  defaultDiagnosticOpts = Ghc.NoDiagnosticOpts
#endif

-- | Diagnostic thrown when config parsing fails
newtype ConfigParseFailDiag = ConfigParseFailDiag String

instance Ghc.Diagnostic ConfigParseFailDiag where
  type DiagnosticOpts ConfigParseFailDiag = Ghc.NoDiagnosticOpts
  diagnosticMessage _ (ConfigParseFailDiag err) = Ghc.mkSimpleDecorated $
    Ghc.text err
  diagnosticReason _ = Ghc.ErrorWithoutFlag
  diagnosticHints _ = []
  diagnosticCode _ = Nothing
#if !MIN_VERSION_ghc(9,8,0)
  defaultDiagnosticOpts = Ghc.NoDiagnosticOpts
#endif

--------------------------------------------------------------------------------
-- Modify source
--------------------------------------------------------------------------------

-- | Parse the given module file. Accounts for CPP comments
parseModule
  :: Ghc.HscEnv
  -> Ghc.DynFlags
  -> String
  -> IO (EP.ParseResult Ghc.ParsedSource, Bool)
parseModule env dynFlags filePath = EP.ghcWrapper Paths.libdir $ do
  Ghc.setSession env { Ghc.hsc_dflags = dynFlags }
  res <- EP.parseModuleEpAnnsWithCppInternal EP.defaultCppOptions dynFlags filePath
  let eCppComments = fmap (\(c, _, _) -> c) res
      hasCpp = case eCppComments of
                 Right cs -> not $ null cs
                 _ -> False
  pure
    ( liftA2 EP.insertCppComments
        (EP.postParseTransform res)
        eCppComments
    , hasCpp
    )

modifyModule
  :: Ghc.ParsedSource
  -> Bool
  -> NE.NonEmpty MissingThing
  -> FilePath
  -> IO ()
modifyModule parsedMod usesCpp neMissing filePath = do
  let updatedAst = modifyAST neMissing parsedMod
  -- If the source contains CPP, newlines are appended
  -- to the end of the file when exact printing. The simple
  -- solution is to remove trailing newlines after exact printing
  -- if the source contains CPP comments.
      removeTrailingNewlines
        | usesCpp =
            reverse . ('\n' :) . dropWhile (== '\n') . reverse
        | otherwise = id
      printed = removeTrailingNewlines $ EP.exactPrint updatedAst
  writeFile filePath printed

modifyAST :: NE.NonEmpty MissingThing -> Ghc.ParsedSource -> Ghc.ParsedSource
modifyAST missingThings parsedSource =
  let
    Ghc.L modLoc hsMod = EP.makeDeltaAst parsedSource
    (qualMods, unqualIds) = partitionEithers $
      NE.toList missingThings >>= \case
        MissingModule m -> [Left m]
        MissingId i -> [Right (importByMod i, [i])]
    newQualImports = mkQualImport <$>
      zip (null updatedImports : repeat False)
          qualMods
    (newUnqualIds, updatedImports) =
      addIdsToExistingImports
        (M.fromListWith (<>) unqualIds)
        (Ghc.hsmodImports hsMod)
    newUnqualImports = mkUnqualImport <$>
      zip ((null updatedImports && null newQualImports) : repeat False)
          (M.toList newUnqualIds)
    importSrcSpan isFirst =
      let lineDelta = if isFirst then 2 else 1
       in Ghc.noAnnSrcSpanDP' $ Ghc.DifferentLine lineDelta 0
    mkQualImport (isFirst, qualMod) =
      let mn = Ghc.mkModuleName $ T.unpack (modName qualMod)
       in Ghc.L (importSrcSpan isFirst)
          (Ghc.simpleImportDecl mn)
            { Ghc.ideclQualified = Ghc.QualifiedPre
            , Ghc.ideclAs = Ghc.L (Ghc.noAnnSrcSpanDP' $ Ghc.SameLine 1)
                          . Ghc.mkModuleName . T.unpack <$> modQual qualMod
            , Ghc.ideclName = Ghc.L (Ghc.noAnnSrcSpanDP' $ Ghc.SameLine 1) mn
            , Ghc.ideclExt = Ghc.XImportDeclPass importEpAnn Ghc.NoSourceText False
            }
    mkUnqualImport :: (Bool, (T.Text, [UnqualIdentifier])) -> Ghc.LImportDecl Ghc.GhcPs
    mkUnqualImport (isFirst, (moName, ids)) =
      let mn = Ghc.mkModuleName $ T.unpack moName
          impList = addCommas $ uncurry mkIE <$>
            zip
              (True : repeat False)
              (associateUnqualIds ids)
          impListAnn = (Ghc.noAnnSrcSpanDP' $ Ghc.SameLine 0)
            { Ghc.anns = (Ghc.noAnn :: Ghc.AnnList (Ghc.EpToken "hiding", [Ghc.EpToken ","]))
              { Ghc.al_brackets = Ghc.ListParens (Ghc.EpTok EP.d1) (Ghc.EpTok EP.d0)
              , Ghc.al_rest = (Ghc.noAnn, [])
              }
            }
       in Ghc.L (importSrcSpan isFirst)
          (Ghc.simpleImportDecl mn)
            { Ghc.ideclName = Ghc.L (Ghc.noAnnSrcSpanDP' $ Ghc.SameLine 1) mn
            , Ghc.ideclExt = Ghc.XImportDeclPass importEpAnn Ghc.NoSourceText False
            , Ghc.ideclImportList = Just (Ghc.Exactly, Ghc.L impListAnn impList)
            }
    importEpAnn = (Ghc.noAnn :: Ghc.EpAnn Ghc.EpAnnImportDecl)
#if MIN_VERSION_ghc(9,12,0)
      { Ghc.anns = Ghc.noAnn
        { Ghc.importDeclAnnImport = Ghc.EpTok Ghc.noAnn
        , Ghc.importDeclAnnQualified = Just (Ghc.EpTok EP.d1)
        , Ghc.importDeclAnnAs = Just (Ghc.EpTok EP.d1)
        }
      }
#endif
  in Ghc.L modLoc hsMod
      { Ghc.hsmodImports = updatedImports ++ newQualImports ++ newUnqualImports
      }

addIdsToExistingImports
  :: M.Map T.Text [UnqualIdentifier]
  -> [Ghc.LImportDecl Ghc.GhcPs]
  -> (M.Map T.Text [UnqualIdentifier], [Ghc.LImportDecl Ghc.GhcPs])
addIdsToExistingImports idMap = foldr go (idMap, [])
  where
    go idecl@(Ghc.L iLoc idec) (iMap, acc)
      | Ghc.NotQualified <- Ghc.ideclQualified idec
      , let hsModName = moduleNameToText . Ghc.unLoc $ Ghc.ideclName idec
      , Just ids <- M.lookup hsModName iMap
      = case Ghc.ideclImportList idec of
          Just (Ghc.Exactly, Ghc.L idsLoc existingIEs) ->
            let
              (idsWithParent, idsWithoutParent) = partitionEithers $ do
                i <- ids
                case parentTy i of
                  Nothing -> [Right i]
                  Just parent -> [Left (idLabel parent, [i])]
              idsWithParentMap = M.fromListWith (<>) idsWithParent
              (idsWithParentToAdd, updatedExistingIEs) =
                addIdsToExistingIEs idsWithParentMap existingIEs
              idsWithoutParentDeduped =
                filter (\x -> M.notMember (identifier x) idsWithParentMap) idsWithoutParent
              newIEs = addCommas $ uncurry mkIE <$>
                zip
                  (True : repeat False)
                  (associateUnqualIds (idsWithoutParentDeduped ++ fold idsWithParentToAdd))
              updatedImportList = (Ghc.Exactly, Ghc.L idsLoc $ updatedExistingIEs ++ newIEs)
              newDecl = idec { Ghc.ideclImportList = Just updatedImportList }
             in (M.delete hsModName iMap, Ghc.L iLoc newDecl : acc)
          _ -> (iMap, idecl : acc)
    go idecl (iMap, acc)
      = (iMap, idecl : acc)

associateUnqualIds :: [UnqualIdentifier] -> [(IdInfo, [IdInfo])]
associateUnqualIds ids = M.toList . M.fromListWith (<>) $ do
  i <- ids
  case parentTy i of
    Nothing -> [(IdInfo (identifier i) (isOperator i), [])]
    Just pt -> [(pt, [IdInfo (identifier i) (isOperator i)])]

mkIE :: Bool -> (IdInfo, [IdInfo]) -> Ghc.LIE Ghc.GhcPs
mkIE isFirstItem (parentId, children) = Ghc.L (ieLoc isFirstItem) $
  case children of
    [] -> Ghc.IEVar Nothing (mkIEWrappedName True parentId) Nothing
    _ ->
      let ieThingWithAnn = (Ghc.EpTok EP.d0, Ghc.noAnn, Ghc.noAnn, Ghc.EpTok EP.d0)
       in Ghc.IEThingWith (Nothing, ieThingWithAnn)
           (mkIEWrappedName True parentId)
           Ghc.NoIEWildcard
           (addCommas $ uncurry mkIEWrappedName <$> zip (True : repeat False) children)
           Nothing
  where
  ieLoc isFirst = Ghc.noAnnSrcSpanDP' $ Ghc.SameLine (if isFirst then 0 else 1)

mkIEWrappedName :: Bool -> IdInfo -> Ghc.LIEWrappedName Ghc.GhcPs
mkIEWrappedName isFirst i =
  Ghc.L ieLoc . Ghc.IEName Ghc.noExtField . Ghc.L (nameAnn $ idIsOp i)
    . txtToRdrName $ idLabel i
  where
    nameAnn :: Bool -> Ghc.EpAnn Ghc.NameAnn
    nameAnn True = (Ghc.noAnn :: Ghc.EpAnn Ghc.NameAnn)
      { Ghc.anns = Ghc.NameAnn
        { Ghc.nann_adornment =
            Ghc.NameParens (Ghc.EpTok EP.d0) (Ghc.EpTok EP.d0)
        , Ghc.nann_name = Ghc.noAnn
        , Ghc.nann_trailing  = []
        }
      }
    nameAnn False = Ghc.noAnn
    ieLoc = Ghc.noAnnSrcSpanDP' $ Ghc.SameLine (if isFirst then 0 else 1)


addCommas :: [Ghc.GenLocated Ghc.SrcSpanAnnA e] -> [Ghc.GenLocated Ghc.SrcSpanAnnA e]
addCommas [] = []
addCommas [x] = [x]
addCommas (Ghc.L ann x : xs) = Ghc.L (EP.addComma ann) x : addCommas xs

addIdsToExistingIEs
  :: M.Map T.Text [UnqualIdentifier] -- keyed by parent ty name
  -> [Ghc.LIE Ghc.GhcPs]
  -> (M.Map T.Text [UnqualIdentifier], [Ghc.LIE Ghc.GhcPs])
addIdsToExistingIEs idsMap = foldr go (idsMap, [])
  where
    go (Ghc.L ieLoc (Ghc.IEThingWith x name wc children Nothing))
       (idMap, acc)
      | Ghc.IEName _ (Ghc.L _ rdrName) <- Ghc.unLoc name
      , let nameTxt = T.pack $ EP.rdrName2String rdrName
      , Just ids <- M.lookup nameTxt idMap
      , let newChildren = addCommas $
              uncurry mkIEWrappedName . fmap toIdInfo <$>
              zip (null children : repeat False) ids
            newLie = Ghc.L ieLoc (Ghc.IEThingWith x name wc (children ++ newChildren) Nothing)
      = ( M.delete nameTxt idMap
        , newLie : acc
        )
    go lie (idMap, acc) = (idMap, lie : acc)

txtToRdrName :: T.Text -> Ghc.RdrName
txtToRdrName = Ghc.mkRdrUnqual . Ghc.mkVarOcc . T.unpack

toIdInfo :: UnqualIdentifier -> IdInfo
toIdInfo i = IdInfo
  { idLabel = identifier i
  , idIsOp = isOperator i
  }

--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------

data ConfigCache = ConfigCache
  { cachedConfig :: !Config
  , localModTime :: !(Maybe UTCTime)
  , homeModTime :: !(Maybe UTCTime)
  }

-- | A global ref used to cache the config and the mod time of the config file
configCacheRef :: IORef (Maybe ConfigCache)
configCacheRef = unsafePerformIO $ newIORef Nothing
{-# NOINLINE configCacheRef #-}

data Config = Config
  { qualModules :: QualMods
  , unqualIdentifiers :: UnqualIdentifiers
  } deriving (Generic, Show)
    deriving (Semigroup, Monoid) via Generically Config

type QualMods = M.Map T.Text QualMod
data QualMod = QualMod
  { modName :: T.Text
  , modQual :: Maybe T.Text
  } deriving (Show, Eq, Ord)

type UnqualIdentifiers = M.Map T.Text UnqualIdentifier
data UnqualIdentifier = UnqualIdentifier
  { importByMod :: T.Text
  , identifier :: T.Text
  , parentTy :: Maybe IdInfo
  , isOperator :: Bool
  } deriving (Show, Eq, Ord)

data IdInfo = IdInfo
  { idLabel :: T.Text
  , idIsOp :: Bool
  } deriving (Show, Eq, Ord)

localConfigFile, homeConfigFile :: FilePath
localConfigFile = "./.autoimport"
homeConfigFile = "~/.autoimport"

resolveConfig :: IO Config
resolveConfig = do
  mCached <- readIORef configCacheRef
  case mCached of
    Just configCache -> do
      mLocalModTime <-
        Dir.doesFileExist localConfigFile >>= \case
          True -> Just <$> Dir.getModificationTime localConfigFile
          False -> pure Nothing
      mHomeModTime <-
        Dir.doesFileExist homeConfigFile >>= \case
          True -> Just <$> Dir.getModificationTime homeConfigFile
          False -> pure Nothing
      if mLocalModTime /= localModTime configCache
         || mHomeModTime /= homeModTime configCache
      then readAndCacheConfig
      else pure $ cachedConfig configCache
    Nothing -> readAndCacheConfig

readAndCacheConfig :: IO Config
readAndCacheConfig = do
  mHomeCfg <- readConfigFile homeConfigFile
  mLocalCfg <- readConfigFile localConfigFile
  case (fst <$> mLocalCfg) <> (fst <$> mHomeCfg) of
    Nothing -> do
      BS8.putStrLn "'.autoimport' file not found by auto-import plugin"
      writeIORef configCacheRef Nothing
      pure mempty
    Just cfg -> do
      let cache = ConfigCache
            { cachedConfig = cfg
            , localModTime = snd <$> mLocalCfg
            , homeModTime = snd <$> mHomeCfg
            }
      writeIORef configCacheRef (Just cache)
      pure cfg

readConfigFile :: FilePath -> IO (Maybe (Config, UTCTime))
readConfigFile file = do
  exists <- Dir.doesFileExist file
  if exists
  then do
    content <- T.readFile file
    modTime <- Dir.getModificationTime file
    case parseConfig file content of
      Left err -> do
        let diag = ConfigParseFailDiag err
            msgEnv =
              Ghc.mkPlainErrorMsgEnvelope
                Ghc.noSrcSpan
                (Ghc.ghcUnknownMessage diag)
        throw $ Ghc.mkSrcErr (Ghc.mkMessages $ Ghc.unitBag msgEnv)
      Right cfg -> pure $ Just (cfg, modTime)
  else pure Nothing

parseConfig :: String -> T.Text -> Either String Config
parseConfig fileName content =
    first Parse.errorBundlePretty $ Parse.runParser parser fileName content
  where
    parser = do
      mconcat
        <$> (Parse.many parseConfigEntry <* Parse.space <* Parse.eof)

parseConfigEntry :: Parse.Parsec Void T.Text Config
parseConfigEntry = do
  moName <- parseModName
  config <-
    Parse.try ((\ids -> mempty
      {unqualIdentifiers = M.fromList $ (\i -> (identifier i, i)) <$> ids}
         ) <$> parseUnqualIds moName)
    <|> ((\qualMod -> mempty
      {qualModules = M.singleton (fromMaybe (modName qualMod) (modQual qualMod)) qualMod}
       ) <$> parseQualMod moName)
  _ <- Parse.hspace <* Parse.optional Parse.eol
  pure config

parseQualMod :: T.Text -> Parse.Parsec Void T.Text QualMod
parseQualMod moName = do
  mQual <- Parse.optional . Parse.try $ do
    Parse.hspace1 *> Parse.string "as" *> Parse.hspace1
    parseModName
  pure $ QualMod moName mQual

parseModName :: Parse.Parsec Void T.Text T.Text
parseModName = T.intercalate "." <$> Parse.sepBy1 parseSegment (Parse.char '.')
  where
    parseSegment = do
      h <- Parse.upperChar
      rest <- Parse.many (Parse.satisfy (\c -> Char.isAlphaNum c || c == '_'))
        Parse.<?> "module name chars"
      pure . T.pack $ h : rest

parseUnqualIds :: T.Text -> Parse.Parsec Void T.Text [UnqualIdentifier]
parseUnqualIds moName = concat <$> do
  Parse.hspace
  Parse.between (Parse.char '(' <* Parse.hspace) (Parse.char ')' <* Parse.hspace)
    (Parse.sepBy1 (parseIdentifier moName) (Parse.char ',' <* Parse.hspace))

parseIdentifier :: T.Text -> Parse.Parsec Void T.Text [UnqualIdentifier]
parseIdentifier moName = do
  (parent, parentIsOp) <- identP <|> operatorP
  let parentId =
        UnqualIdentifier
        { importByMod = moName
        , identifier = parent
        , parentTy = Nothing
        , isOperator = parentIsOp
        }
  if parentIsOp || T.all Char.isUpper (T.take 1 parent)
  then do
    Parse.hspace
    mChildIds <- Parse.optional childIdsP
    case mChildIds of
      Nothing -> pure [ parentId ]
      Just childIds -> pure . (parentId :) $
        (\(cid, isOp) -> UnqualIdentifier
          { importByMod = moName
          , identifier = cid
          , parentTy = Just (IdInfo parent parentIsOp)
          , isOperator = isOp
          }) <$> childIds
  else pure [ parentId ]

  where
    identP = do
      fc <- Parse.letterChar
      rest <- Parse.many (Parse.satisfy (\c -> Char.isAlphaNum c || c `elem` ['_', '\'', '#']))
        Parse.<?> "identifier chars"
      pure (T.pack $ fc : rest, False)
    operatorP = (, True) . T.pack <$>
      Parse.between (Parse.char '(' <* Parse.hspace) (Parse.char ')' <* Parse.hspace)
        (Parse.some (Parse.oneOf (":!#$%&*+./<=>?@\\^|-~" :: String) Parse.<?> "operator char"))
    childIdsP =
      Parse.between (Parse.char '(' <* Parse.hspace) (Parse.char ')' <* Parse.hspace) $
        Parse.sepBy1 (identP <|> operatorP) (Parse.char ',' <* Parse.hspace)
