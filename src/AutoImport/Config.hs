{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
module AutoImport.Config
  ( ConfigCache(..)
  , Config(..)
  , QualMods
  , QualMod(..)
  , UnqualIdentifiers
  , UnqualIdentifier(..)
  , IdInfo(..)
  , resolveConfig
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (guard)
import           Control.Exception (throw)
import           Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Char as Char
import           Data.Functor (void)
import           Data.IORef
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time (UTCTime)
import           Data.Void
import           GHC.Generics (Generic, Generically(..))
import qualified System.Directory as Dir
import           System.IO.Unsafe (unsafePerformIO)
import qualified Text.Megaparsec as Parse
import qualified Text.Megaparsec.Char as Parse
import qualified Text.Megaparsec.Char.Lexer as Parse (skipLineComment)

import qualified AutoImport.GhcFacade as Ghc

-- | A global ref used to cache the config and the mod time of the config file
configCacheRef :: IORef (Maybe ConfigCache)
configCacheRef = unsafePerformIO $ newIORef Nothing
{-# NOINLINE configCacheRef #-}

data ConfigCache = ConfigCache
  { cachedConfig :: !Config
  , localModTime :: !(Maybe UTCTime)
  , homeModTime :: !(Maybe UTCTime)
  }

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
      _ <- Parse.optional skipSpaceNoIndent
      mconcat
        <$> (Parse.many p <* Parse.space <* Parse.eof)
    p = ((parseConfigEntry <* Parse.optional parseLineComment)
          <|> (mempty <$ parseLineComment))
        <* skipSpaceNoIndent

parseConfigEntry :: Parse.Parsec Void T.Text Config
parseConfigEntry = do
  moName <- parseModName

  let parseUnqualIdsEntry =
        (\ids -> mempty
          {unqualIdentifiers = M.fromList $ (\i -> (identifier i, i)) <$> ids}
        ) <$> parseUnqualIds moName
      parseQualModEntry =
        (\qualMod -> mempty
          {qualModules = M.singleton (fromMaybe (modName qualMod) (modQual qualMod)) qualMod}
        ) <$> parseModQual moName
      selfQualMod = pure mempty
          { qualModules = M.singleton moName (QualMod moName Nothing)}

  ( do
    Parse.try hspaceOrIndent
    parseUnqualIdsEntry <|> parseQualModEntry
    ) <|> selfQualMod

parseModQual :: T.Text -> Parse.Parsec Void T.Text QualMod
parseModQual moName = do
  qual <- do
    Parse.string "as" *> Parse.hspace1
    parseModName
  pure $ QualMod moName (Just qual)

parseModName :: Parse.Parsec Void T.Text T.Text
parseModName = T.intercalate "." <$> Parse.sepBy1 parseSegment (Parse.char '.')
  where
    parseSegment = do
      h <- Parse.upperChar
      rest <- Parse.many (Parse.satisfy (\c -> Char.isAlphaNum c || c == '_'))
        Parse.<?> "module name chars"
      pure . T.pack $ h : rest

parseUnqualIds :: T.Text -> Parse.Parsec Void T.Text [UnqualIdentifier]
parseUnqualIds moName = concat <$>
  Parse.between (Parse.char '(' <* hspaceOrIndent) (Parse.char ')')
    (Parse.sepBy1 (parseIdentifier moName <* hspaceOrIndent) (Parse.char ',' <* hspaceOrIndent))

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
    hspaceOrIndent
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
      fc <- Parse.char '_' <|> Parse.letterChar
      rest <- Parse.many (Parse.satisfy (\c -> Char.isAlphaNum c || c `elem` ['_', '\'', '#']))
        Parse.<?> "identifier chars"
      pure (T.pack $ fc : rest, False)
    operatorP = (, True) . T.pack <$>
      Parse.between (Parse.char '(' <* Parse.hspace) (Parse.hspace *> Parse.char ')')
        (Parse.some (Parse.oneOf (":!#$%&*+./<=>?@\\^|-~" :: String) Parse.<?> "operator char"))
    childIdsP =
      Parse.between (Parse.char '(' <* hspaceOrIndent) (Parse.char ')') $
        Parse.sepBy1 (identP <|> operatorP <* hspaceOrIndent)
                     (Parse.char ',' <* hspaceOrIndent)

parseLineComment :: Parse.Parsec Void T.Text ()
parseLineComment = do
  Parse.hspace
  Parse.skipLineComment "--"

-- Skip horizontal and vertical space until unindented text is found
skipSpaceNoIndent :: Parse.Parsec Void T.Text ()
skipSpaceNoIndent =
  void (Parse.some (Parse.hspace *> Parse.eol))
    <|> Parse.eof

-- | Parse any amount of whitespace not ending in a newline
hspaceOrIndent :: Parse.Parsec Void T.Text ()
hspaceOrIndent = do
  Parse.hspace
  (do void Parse.eol
      s : _ <- reverse <$> Parse.some Parse.spaceChar
      guard (s /= '\n')
    ) <|> pure ()

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
