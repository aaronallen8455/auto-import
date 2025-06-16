{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
module AutoImport
  ( plugin
  ) where

import           Control.Exception (try, throw)
import           Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Char as Char
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
            mkMissingModError msgEnv =
              case Ghc.errMsgDiagnostic msgEnv of
                Ghc.GhcTcRnMessage
                    (Ghc.TcRnMessageWithInfo _
                      (Ghc.TcRnMessageDetailed _
                        (Ghc.TcRnNotInScope Ghc.NotInScope _ [Ghc.MissingModule missingMod] _))
                    )
                  | let modTxt = TE.decodeUtf8 . Ghc.bytesFS $ Ghc.moduleNameFS missingMod
                  , Just (modName, mQuali) <- M.lookup modTxt autoImportCfg
                  -> Right (modName, mQuali)
                _ -> Left msgEnv
            (_otherDiags, missingMods) =
              Ghc.partitionBagWith mkMissingModError (Ghc.getMessages msgs)
        case NE.nonEmpty (toList missingMods) of
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
  -> NE.NonEmpty (T.Text, Maybe T.Text)
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

modifyAST :: NE.NonEmpty (T.Text, Maybe T.Text) -> Ghc.ParsedSource -> Ghc.ParsedSource
modifyAST missingMods = fmap addImports . EP.makeDeltaAst
  where
    addImports hsMod = hsMod
      { Ghc.hsmodImports = Ghc.hsmodImports hsMod ++ newImports hsMod
      }
    newImports hsMod = mkImport <$>
      zip (null (Ghc.hsmodImports hsMod) : repeat False)
          (NE.toList missingMods)
    importSrcSpan isFirst =
      let lineDelta = if isFirst then 2 else 1
       in Ghc.noAnnSrcSpanDP' $ Ghc.DifferentLine lineDelta 0
    mkImport (isFirst, (modName, mQual)) = Ghc.L (importSrcSpan isFirst) $
      (Ghc.simpleImportDecl . Ghc.mkModuleName $ T.unpack modName)
        { Ghc.ideclQualified = Ghc.QualifiedPre
        , Ghc.ideclAs = Ghc.L Ghc.noSrcSpanA . Ghc.mkModuleName . T.unpack <$> mQual
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

type Config = M.Map T.Text (T.Text, Maybe T.Text)

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
    parser = M.fromList
           . map (\(name, mQual) -> (fromMaybe name mQual, (name, mQual)))
           <$> (Parse.many parseLine <* Parse.space <* Parse.eof)

parseLine :: Parse.Parsec Void T.Text (T.Text, Maybe T.Text)
parseLine = do
  modName <- parseModName
  mQual <- Parse.optional . Parse.try $ do
    Parse.hspace1 *> Parse.string "as" *> Parse.hspace1
    parseModName
  _ <- Parse.hspace <* Parse.optional Parse.eol
  pure (modName, mQual)

parseModName :: Parse.Parsec Void T.Text T.Text
parseModName = T.intercalate "." <$> Parse.sepBy1 parseSegment (Parse.char '.')
  where
    parseSegment = do
      h <- Parse.satisfy Char.isUpper Parse.<?> "uppercase char"
      rest <- Parse.many (Parse.satisfy (\c -> Char.isAlphaNum c || c == '_'))
        Parse.<?> "module name chars"
      pure . T.pack $ h : rest
