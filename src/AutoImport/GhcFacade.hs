{-# LANGUAGE CPP #-}
module AutoImport.GhcFacade
  ( module Ghc
  , epAnnSrcSpan
  , determineStartLine
  , ann'
  ) where

import           Control.Applicative ((<|>))

import           GHC as Ghc
import           GHC.Data.Bag as Ghc
import           GHC.Data.FastString as Ghc
import           GHC.Driver.Env as Ghc
import           GHC.Driver.Errors.Types as Ghc
import           GHC.Driver.Hooks as Ghc
import           GHC.Driver.Pipeline.Execute as Ghc
import           GHC.Driver.Pipeline.Phases as Ghc
import           GHC.Driver.Plugins as Ghc
import           GHC.Driver.Monad as Ghc
import           GHC.Tc.Errors.Types as Ghc
import           GHC.Types.Error as Ghc
import           GHC.Types.SourceError as Ghc
import           GHC.Types.SrcLoc as Ghc
import           GHC.Unit.Module.ModSummary as Ghc
import           GHC.Utils.Error as Ghc
import           GHC.Utils.Outputable as Ghc
import           GHC.Driver.DynFlags as Ghc

#if MIN_VERSION_ghc(9,10,0)
ann' :: Ghc.EpAnn ann -> Ghc.EpAnn ann
ann' = id
#else
ann' :: Ghc.SrcSpanAnnA -> Ghc.EpAnn Ghc.AnnListItem
ann' = Ghc.ann
#endif

#if MIN_VERSION_ghc(9,10,0)
determineStartLine :: EpAnn ann -> Maybe Int
#else
determineStartLine :: Ghc.SrcSpanAnnA -> Maybe Int
#endif
determineStartLine epAnn = pred <$>
    (min mCommentLine mElemLine <|> mElemLine)
  where
    mElemLine =
      Ghc.srcSpanStartLine <$> Ghc.srcSpanToRealSrcSpan (Ghc.locA epAnn)
#if MIN_VERSION_ghc(9,12,0)
    getRSS = Ghc.epaLocationRealSrcSpan
#else
    getRSS = Ghc.anchor
#endif
    mCommentLine =
      case ann' epAnn of
        x@Ghc.EpAnn{} -> case Ghc.comments x of
          Ghc.EpaComments (Ghc.L comLoc _ : _) ->
            Just . Ghc.srcSpanStartLine $ getRSS comLoc
          Ghc.EpaCommentsBalanced (Ghc.L comLoc _ : _) _ ->
            Just . Ghc.srcSpanStartLine $ getRSS comLoc
          _ -> Nothing
#if !MIN_VERSION_ghc(9,10,0)
        Ghc.EpAnnNotUsed -> Nothing
#endif

epAnnSrcSpan :: Ghc.EpAnn ann -> Maybe Ghc.RealSrcSpan
epAnnSrcSpan epAnn = do
#if MIN_VERSION_ghc(9,10,0)
  Ghc.EpaSpan (Ghc.RealSrcSpan srcSpan _) <- Just $ Ghc.entry epAnn
#else
  Ghc.Anchor srcSpan _ <- Just $ Ghc.entry epAnn
#endif
  Just srcSpan
