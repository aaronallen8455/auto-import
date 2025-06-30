{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
module AutoImport
  ( plugin
  ) where

import           Control.Exception (try, throw)
import           Data.Containers.ListUtils (nubOrd)
import           Data.Either (partitionEithers)
import           Data.Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified GHC.LanguageExtensions as LangExt
import qualified GHC.Paths as Paths
import qualified Language.Haskell.GHC.ExactPrint as EP
import qualified Language.Haskell.GHC.ExactPrint.Parsers as EP
import qualified Language.Haskell.GHC.ExactPrint.Utils as EP

import           AutoImport.Config
import qualified AutoImport.GhcFacade as Ghc

--------------------------------------------------------------------------------
-- Plugin
--------------------------------------------------------------------------------

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.driverPlugin = \args hscEnv -> pure $ addHscHook args hscEnv
  , Ghc.pluginRecompile = Ghc.purePlugin
  }

addHscHook :: [Ghc.CommandLineOption] -> Ghc.HscEnv -> Ghc.HscEnv
addHscHook args hscEnv = hscEnv
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
        autoImportCfg <- resolveConfig (asum $ parseConfigPathArg <$> args)
        let msgs = case eTcRes of
                     Left (Ghc.SourceError m) -> m
                     Right (_, m) -> m
            missingThings =
              Ghc.mapMaybeBag (missingThingFromMsg autoImportCfg) (Ghc.getMessages msgs)
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

parseConfigPathArg :: Ghc.CommandLineOption -> Maybe FilePath
parseConfigPathArg ('-':'-':'c':'f':'g':'=':path) = Just path
parseConfigPathArg _ = Nothing

moduleNameToText :: Ghc.ModuleName -> T.Text
moduleNameToText = TE.decodeUtf8 . Ghc.bytesFS . Ghc.moduleNameFS

data MissingThing
  = MissingModule QualMod
  | MissingId UnqualIdentifier
  deriving (Eq, Ord)

missingThingFromMsg :: Config -> Ghc.MsgEnvelope Ghc.GhcMessage -> Maybe MissingThing
missingThingFromMsg autoImportCfg msgEnv =
  case Ghc.errMsgDiagnostic msgEnv of
    Ghc.GhcTcRnMessage
        (Ghc.TcRnMessageWithInfo _
          (Ghc.TcRnMessageDetailed _
            (Ghc.TcRnNotInScope Ghc.NotInScope _ [Ghc.MissingModule missingMod] _))
        )
      | let modTxt = moduleNameToText missingMod
      , Just qualMod <- M.lookup modTxt (qualModules autoImportCfg)
      -> Just $ MissingModule qualMod
    Ghc.GhcTcRnMessage
        (Ghc.TcRnMessageWithInfo _
          (Ghc.TcRnMessageDetailed _
            (Ghc.TcRnNotInScope Ghc.NotInScope rdrName [] hints))
        )
      | Just unqualId <- M.lookup (T.pack $ EP.rdrName2String rdrName) (unqualIdentifiers autoImportCfg)
      , let isDataKindExtHint = \case
              Ghc.SuggestExtension (Ghc.SuggestSingleExtension _ LangExt.DataKinds) -> True
              _ -> False
        -- If -XDataKinds is suggested, that means the import already exists
        -- and should not be added again.
      , not (any isDataKindExtHint hints)
      -> Just $ MissingId unqualId
    Ghc.GhcTcRnMessage
        (Ghc.TcRnMessageWithInfo _
          (Ghc.TcRnMessageDetailed _
            (Ghc.TcRnSolverReport' report )
        ))
      | Ghc.ReportHoleError hole _
          <- Ghc.reportContent report
      , Just unqualId <- M.lookup (T.pack . EP.rdrName2String $ Ghc.hole_occ hole) (unqualIdentifiers autoImportCfg)
      -> Just $ MissingId unqualId
    _ -> Nothing

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
            , Ghc.ideclExt = Ghc.XImportDeclPass importEpAnnQual Ghc.NoSourceText False
            }
    mkUnqualImport :: (Bool, (T.Text, [UnqualIdentifier])) -> Ghc.LImportDecl Ghc.GhcPs
    mkUnqualImport (isFirst, (moName, ids)) =
      let mn = Ghc.mkModuleName $ T.unpack moName
          impList = addCommas $ uncurry mkIE <$>
            zip
              (True : repeat False)
              (associateUnqualIds ids)
       in Ghc.L (importSrcSpan isFirst)
          (Ghc.simpleImportDecl mn)
            { Ghc.ideclName = Ghc.L (Ghc.noAnnSrcSpanDP' $ Ghc.SameLine 1) mn
            , Ghc.ideclExt = Ghc.XImportDeclPass Ghc.importEpAnn Ghc.NoSourceText False
            , Ghc.ideclImportList = Just (Ghc.Exactly, Ghc.L Ghc.importListAnn impList)
            }
    importEpAnnQual = (Ghc.noAnn :: Ghc.EpAnn Ghc.EpAnnImportDecl)
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
addIdsToExistingImports = List.mapAccumR go
  where
    go iMap (Ghc.L iLoc idec)
      | Ghc.NotQualified <- Ghc.ideclQualified idec
      , let hsModName = moduleNameToText . Ghc.unLoc $ Ghc.ideclName idec
      , Just ids <- M.lookup hsModName iMap
      , Just (Ghc.Exactly, Ghc.L idsLoc existingIEs) <- Ghc.ideclImportList idec
      = let (idsWithParent, idsWithoutParent) = partitionEithers $ do
               i <- ids
               case parentTy i of
                 Nothing -> [Right i]
                 Just parent -> [Left (idLabel parent, [i])]
            idsWithParentMap = M.fromListWith (<>) idsWithParent
            (idsWithParentToAdd, updatedExistingIEs) =
              addIdsToExistingIEs idsWithParentMap existingIEs
            idsWithoutParentDeduped =
              filter (\x -> M.notMember (identifier x) idsWithParentMap) idsWithoutParent
            newIEs = uncurry mkIE <$>
              zip
                (null updatedExistingIEs : repeat False)
                (associateUnqualIds (idsWithoutParentDeduped ++ fold idsWithParentToAdd))
            updatedImportList = (Ghc.Exactly, Ghc.L idsLoc . addCommas $ updatedExistingIEs ++ newIEs)
            newDecl = idec { Ghc.ideclImportList = Just updatedImportList }
        in (M.delete hsModName iMap, Ghc.L iLoc newDecl)
    go iMap idecl = (iMap, idecl)

associateUnqualIds :: [UnqualIdentifier] -> [(IdInfo, [IdInfo])]
associateUnqualIds ids = M.toList . M.fromListWith (<>) $ do
  i <- ids
  case parentTy i of
    Nothing -> [(IdInfo (identifier i) (isOperator i), [])]
    Just pt -> [(pt, [IdInfo (identifier i) (isOperator i)])]

mkIE :: Bool -> (IdInfo, [IdInfo]) -> Ghc.LIE Ghc.GhcPs
mkIE isFirstItem (parentId, children) = Ghc.L (ieLoc isFirstItem) $
  case children of
    [] -> Ghc.IEVar' (mkIEWrappedName True parentId)
    _ -> Ghc.IEThingWith' Ghc.ieThingWithAnn
           (mkIEWrappedName True parentId)
           Ghc.NoIEWildcard
           (addCommas $ uncurry mkIEWrappedName <$> zip (True : repeat False) children)
  where
  ieLoc isFirst = Ghc.noAnnSrcSpanDP' $ Ghc.SameLine (if isFirst then 0 else 1)

mkIEWrappedName :: Bool -> IdInfo -> Ghc.LIEWrappedName Ghc.GhcPs
mkIEWrappedName isFirst i =
  Ghc.L ieLoc . Ghc.IEName Ghc.noExtField . Ghc.L (Ghc.nameAnn $ idIsOp i)
    . txtToRdrName $ idLabel i
  where
    ieLoc = Ghc.noAnnSrcSpanDP' $ Ghc.SameLine (if isFirst then 0 else 1)

addCommas :: [Ghc.GenLocated Ghc.SrcSpanAnnA e] -> [Ghc.GenLocated Ghc.SrcSpanAnnA e]
addCommas [] = []
addCommas [x] = [x]
addCommas (Ghc.L ann x : xs) =
  Ghc.L (if Ghc.hasTrailingComma ann then ann else EP.addComma ann) x
  : addCommas xs

addIdsToExistingIEs
  :: M.Map T.Text [UnqualIdentifier] -- keyed by parent ty name
  -> [Ghc.LIE Ghc.GhcPs]
  -> (M.Map T.Text [UnqualIdentifier], [Ghc.LIE Ghc.GhcPs])
addIdsToExistingIEs = List.mapAccumR go
  where
    go idMap (Ghc.L ieLoc (Ghc.IEThingAbs' name))
      | Ghc.IEName _ (Ghc.L _ rdrName) <- Ghc.unLoc name
      , let nameTxt = T.pack $ EP.rdrName2String rdrName
      , Just ids <- M.lookup nameTxt idMap
      , let newChildren =
              uncurry mkIEWrappedName . fmap toIdInfo <$>
              zip (True : repeat False) ids
            newLie = Ghc.L ieLoc
              (Ghc.IEThingWith' Ghc.ieThingWithAnn name Ghc.NoIEWildcard (addCommas newChildren))
      = (M.delete nameTxt idMap, newLie)
    go idMap (Ghc.L ieLoc (Ghc.IEThingWith' x name wc children))
      | Ghc.IEName _ (Ghc.L _ rdrName) <- Ghc.unLoc name
      , let nameTxt = T.pack $ EP.rdrName2String rdrName
      , Just ids <- M.lookup nameTxt idMap
      , let newChildren =
              uncurry mkIEWrappedName . fmap toIdInfo <$>
              zip (null children : repeat False) ids
            newLie = Ghc.L ieLoc (Ghc.IEThingWith' x name wc (addCommas $ children ++ newChildren))
      = (M.delete nameTxt idMap, newLie)
    go idMap lie = (idMap, lie)

txtToRdrName :: T.Text -> Ghc.RdrName
txtToRdrName = Ghc.mkRdrUnqual . Ghc.mkVarOcc . T.unpack

toIdInfo :: UnqualIdentifier -> IdInfo
toIdInfo i = IdInfo
  { idLabel = identifier i
  , idIsOp = isOperator i
  }
