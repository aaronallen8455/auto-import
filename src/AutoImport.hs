{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
module AutoImport
  ( plugin
  ) where

import           Control.Applicative
import           Control.Exception (try, throw)
import           Control.Monad
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
import           Data.Void
import qualified System.Directory as Dir
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
        insertLineRef <- newIORef Nothing :: IO (IORef (Maybe InsertLine))
        let parsePlugin =
              Ghc.defaultPlugin
              { Ghc.parsedResultAction = \_ _modSum parsedResult -> do
                  let !mInsertLine = getLineToInsertAt . Ghc.unLoc . Ghc.hpm_module $ Ghc.parsedResultModule parsedResult
                  Ghc.liftIO $ writeIORef insertLineRef mInsertLine
                  pure parsedResult
              }
            staticPlugin = Ghc.StaticPlugin
              { Ghc.spPlugin = Ghc.PluginWithArgs parsePlugin []
#if MIN_VERSION_ghc(9,12,0)
              , Ghc.spInitialised = True
#endif
              }
            envWithPlugin = env
              { Ghc.hsc_plugins = let ps = Ghc.hsc_plugins env in ps
                { Ghc.staticPlugins = staticPlugin : Ghc.staticPlugins ps }
              }
        eTcRes <- try . runPhaseOrExistingHook $ Ghc.T_Hsc envWithPlugin modSum
        readIORef insertLineRef >>= \case
          Nothing -> either throw pure eTcRes
          Just lineToInsertAt -> do
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
                (otherDiags, missingMods) =
                  Ghc.partitionBagWith mkMissingModError (Ghc.getMessages msgs)
            case NE.nonEmpty (toList missingMods) of
              Nothing -> either throw pure eTcRes
              Just neMissing -> do
                modifySource lineToInsertAt modFile neMissing
                throw (Ghc.SourceError . Ghc.mkMessages $ Ghc.consBag importsAddedErr otherDiags)
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

modifySource :: InsertLine -> FilePath -> NE.NonEmpty (T.Text, Maybe T.Text) -> IO ()
modifySource (InsertLine addNewLine lineNo) modFile missingMods = do
  exists <- Dir.doesFileExist modFile
  when exists $ do
    content <- T.readFile modFile
    let (before, after) = splitAt lineNo $ T.lines content
        importStmts = mkImportStmt <$> missingMods
    T.writeFile modFile . T.unlines $ before <> toList importStmts
      <> (if addNewLine then ("" :) else id) after

data InsertLine =
  InsertLine
    !Bool -- True => Add blank line after the imports
    !Int

getLineToInsertAt :: Ghc.HsModule Ghc.GhcPs -> Maybe InsertLine
getLineToInsertAt hsMod = mImportLn <|> mDeclLn
  where
    mImportLn = do
      Ghc.L epAnn _firstImp : _ <- Just $ Ghc.hsmodImports hsMod
      InsertLine False <$> determineStartLine (Ghc.ann' epAnn)
    mDeclLn = do
      Ghc.L epAnn _firstDecl : _ <- Just $ Ghc.hsmodDecls hsMod
      InsertLine True <$> determineStartLine (Ghc.ann' epAnn)
    determineStartLine epAnn = pred <$>
      case Ghc.comments epAnn of
        Ghc.EpaComments (Ghc.L _ com : _) ->
          Just . Ghc.srcSpanStartLine $ Ghc.ac_prior_tok com
        Ghc.EpaCommentsBalanced (Ghc.L _ com : _) _ ->
          Just . Ghc.srcSpanStartLine $ Ghc.ac_prior_tok com
        _ -> Ghc.srcSpanStartLine <$> Ghc.epAnnSrcSpan epAnn

mkImportStmt :: (T.Text, Maybe T.Text) -> T.Text
mkImportStmt (modName, mQual) =
  "import qualified " <> modName <> foldMap (" as " <>) mQual

--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------

type Config = M.Map T.Text (T.Text, Maybe T.Text)

resolveConfig :: IO Config
resolveConfig = do
  mHomeCfg <- readConfigFile "~/.autoimport.cfg"
  mLocalCfg <- readConfigFile "./.autoimport.cfg"
  case mLocalCfg <> mHomeCfg of
    Nothing -> do
      BS8.putStrLn "'.autoimport.cfg' file not found by auto-import plugin"
      pure mempty
    Just cfg -> pure cfg

readConfigFile :: FilePath -> IO (Maybe Config)
readConfigFile file = do
  exists <- Dir.doesFileExist file
  if exists
  then do
    content <- T.readFile file
    case parseConfig file content of
      Left err -> do
        let diag = ConfigParseFailDiag err
            msgEnv =
              Ghc.mkPlainErrorMsgEnvelope
                Ghc.noSrcSpan
                (Ghc.ghcUnknownMessage diag)
        throw $ Ghc.SourceError (Ghc.mkMessages $ Ghc.unitBag msgEnv)
      Right cfg -> pure $ Just cfg
  else pure Nothing

parseConfig :: String -> T.Text -> Either String Config
parseConfig fileName content =
    first Parse.errorBundlePretty $ Parse.runParser parser fileName content
  where
    parser = M.fromList
           . map (\(name, mQual) -> (fromMaybe name mQual, (name, mQual)))
           <$> (Parse.many parseLine <* Parse.eof)

parseLine :: Parse.Parsec Void T.Text (T.Text, Maybe T.Text)
parseLine = do
  modName <- parseModName
  mQual <- Parse.optional $ do
    Parse.space1 *> Parse.string "as" *> Parse.space1
    parseModName
  _ <- Parse.optional Parse.eol
  pure (modName, mQual)

parseModName :: Parse.Parsec Void T.Text T.Text
parseModName = T.intercalate "." <$> Parse.sepBy1 parseSegment (Parse.char '.')
  where
    parseSegment = do
      h <- Parse.satisfy Char.isUpper Parse.<?> "uppercase char"
      rest <- Parse.many (Parse.satisfy Char.isAlphaNum)
      pure . T.pack $ h : rest
