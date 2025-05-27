{-# LANGUAGE CPP #-}
module AutoImport.GhcFacade
  ( module Ghc
  , epAnnSrcSpan
  , ann'
  ) where

import           GHC.Parser.Annotation as Ghc
import           GHC.Data.Bag as Ghc
import           GHC.Data.FastString as Ghc
import           GHC.Driver.Env as Ghc
import           GHC.Driver.Errors.Types as Ghc
import           GHC.Driver.Hooks as Ghc
import           GHC.Driver.Pipeline.Execute as Ghc
import           GHC.Driver.Pipeline.Phases as Ghc
import           GHC.Driver.Plugins as Ghc
import           GHC.Driver.Monad as Ghc
import           GHC.Hs as Ghc
import           GHC.Tc.Errors.Types as Ghc
import           GHC.Types.Error as Ghc
import           GHC.Types.SourceError as Ghc
import           GHC.Types.SrcLoc as Ghc
import           GHC.Unit.Module.ModSummary as Ghc
import           GHC.Utils.Error as Ghc
import           GHC.Utils.Outputable as Ghc

#if MIN_VERSION_ghc(9,10,0)
ann' :: Ghc.EpAnn ann -> Ghc.EpAnn ann
ann' = id
#else
ann' :: Ghc.SrcSpanAnnA -> Ghc.EpAnn Ghc.AnnListItem
ann' = Ghc.ann
#endif

epAnnSrcSpan :: Ghc.EpAnn ann -> Maybe Ghc.RealSrcSpan
epAnnSrcSpan epAnn = do
#if MIN_VERSION_ghc(9,10,0)
  Ghc.EpaSpan (Ghc.RealSrcSpan srcSpan _) <- Just $ Ghc.entry epAnn
#else
  Ghc.Anchor srcSpan _ <- Just $ Ghc.entry epAnn
#endif
  Just srcSpan
