{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
module AutoImport.GhcFacade
  ( module Ghc
  , epAnnSrcSpan
  , determineStartLine
  , ann'
  , noAnnSrcSpanDP'
  , nameParensAdornment
  , ieThingWithAnn
  , importListAnn
  , operatorNameAnn
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
import           GHC.Tc.Types.Constraint as Ghc
import           GHC.Types.Error as Ghc
import           GHC.Types.Name.Occurrence as Ghc
import           GHC.Types.Name.Reader as Ghc
import           GHC.Types.SourceError as Ghc
import           GHC.Types.SourceText as Ghc
import           GHC.Types.SrcLoc as Ghc
import           GHC.Unit.Module.ModSummary as Ghc
import           GHC.Utils.Error as Ghc
import           GHC.Utils.Outputable as Ghc
import           GHC.Utils.Misc as Ghc
#if MIN_VERSION_ghc(9,8,0)
import           GHC.Driver.DynFlags as Ghc
#else
import           GHC.Driver.Flags as Ghc
import           GHC.Driver.Session as Ghc
#endif

import qualified Language.Haskell.GHC.ExactPrint as EP

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

noAnnSrcSpanDP' :: Ghc.DeltaPos -> Ghc.SrcSpanAnnA --EpAnn ann
noAnnSrcSpanDP'
#if MIN_VERSION_ghc(9,10,0)
  = EP.noAnnSrcSpanDP
#else
  = EP.noAnnSrcSpanDP Ghc.noSrcSpan
#endif

nameParensAdornment :: Ghc.NameAdornment
nameParensAdornment =
#if MIN_VERSION_ghc(9,12,0)
  Ghc.NameParens (Ghc.EpTok EP.d0) (Ghc.EpTok EP.d0)
#else
  Ghc.NameParens
#endif

ieThingWithAnn :: Ghc.XIEThingWith Ghc.GhcPs
ieThingWithAnn =
#if MIN_VERSION_ghc(9,12,0)
  (Nothing, (Ghc.EpTok EP.d0, Ghc.noAnn, Ghc.noAnn, Ghc.EpTok EP.d0))
#else
  (Nothing, [Ghc.AddEpAnn Ghc.AnnOpenP EP.d0, Ghc.AddEpAnn Ghc.AnnCloseP EP.d0])
#endif

#if MIN_VERSION_ghc(9,12,0)
importListAnn :: Ghc.EpAnn (Ghc.AnnList (Ghc.EpToken "hiding", [Ghc.EpToken ","]))
#else
importListAnn :: Ghc.EpAnn Ghc.AnnList
#endif
importListAnn =
  (noAnnSrcSpanDP' $ Ghc.SameLine 0)
#if MIN_VERSION_ghc(9,12,0)
    { Ghc.anns = (Ghc.noAnn :: Ghc.AnnList (Ghc.EpToken "hiding", [Ghc.EpToken ","]))
      { Ghc.al_brackets = Ghc.ListParens (Ghc.EpTok EP.d1) (Ghc.EpTok EP.d0)
      , Ghc.al_rest = (Ghc.noAnn, [])
      }
    }
#else
    { Ghc.anns = (Ghc.noAnn :: Ghc.AnnList)
      { Ghc.al_open = Just $ Ghc.AddEpAnn Ghc.AnnOpenP EP.d1
      , Ghc.al_close = Just $ Ghc.AddEpAnn Ghc.AnnCloseP EP.d0
      }
    }
#endif

operatorNameAnn :: Ghc.EpAnn Ghc.NameAnn
operatorNameAnn =
#if MIN_VERSION_ghc(9,12,0)
  (Ghc.noAnn :: Ghc.EpAnn Ghc.NameAnn)
    { Ghc.anns = Ghc.NameAnn
      { Ghc.nann_adornment = Ghc.nameParensAdornment
      , Ghc.nann_name = Ghc.noAnn
      , Ghc.nann_trailing  = []
      }
    }
#else
  (Ghc.noAnn :: Ghc.EpAnn Ghc.NameAnn)
    { Ghc.anns = Ghc.NameAnn
      { Ghc.nann_adornment = nameParensAdornment
      , Ghc.nann_name = Ghc.noAnn
      , Ghc.nann_trailing  = []
      , Ghc.nann_open = Ghc.noAnn
      , Ghc.nann_close = Ghc.noAnn
      }
    }
#endif
