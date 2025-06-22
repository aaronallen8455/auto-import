{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
module AutoImport.GhcFacade
  ( module Ghc
  , ann'
  , noAnnSrcSpanDP'
  , nameParensAdornment
  , ieThingWithAnn
  , importListAnn
  , nameAnn
  , importEpAnn
  , hasTrailingComma
  , pattern IEThingWith'
  , pattern IEVar'
  , pattern TcRnSolverReport'
  , pattern IEThingAbs'
  ) where

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

noAnnSrcSpanDP'
#if MIN_VERSION_ghc(9,10,0)
  :: Ghc.NoAnn ann
  => Ghc.DeltaPos -> Ghc.EpAnn ann
#else
  :: Monoid ann
  => Ghc.DeltaPos -> Ghc.SrcSpanAnn' (Ghc.EpAnn ann)
#endif
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
#elif MIN_VERSION_ghc(9,10,0)
  (Nothing, [Ghc.AddEpAnn Ghc.AnnOpenP EP.d0, Ghc.AddEpAnn Ghc.AnnCloseP EP.d0])
#elif MIN_VERSION_ghc(9,8,0)
  ( Nothing
  , Ghc.EpAnn (Ghc.Anchor Ghc.placeholderRealSpan EP.m0)
      [Ghc.AddEpAnn Ghc.AnnOpenP EP.d0, Ghc.AddEpAnn Ghc.AnnCloseP EP.d0]
      Ghc.emptyComments
  )
#else
  Ghc.EpAnn (Ghc.Anchor Ghc.placeholderRealSpan EP.m0)
    [Ghc.AddEpAnn Ghc.AnnOpenP EP.d0, Ghc.AddEpAnn Ghc.AnnCloseP EP.d0]
    Ghc.emptyComments
#endif

#if MIN_VERSION_ghc(9,12,0)
importListAnn :: Ghc.EpAnn (Ghc.AnnList (Ghc.EpToken "hiding", [Ghc.EpToken ","]))
#elif MIN_VERSION_ghc(9,10,0)
importListAnn :: Ghc.EpAnn Ghc.AnnList
#else
importListAnn :: Ghc.SrcSpanAnn' (Ghc.EpAnn Ghc.AnnList)
#endif
importListAnn =
#if MIN_VERSION_ghc(9,12,0)
  (noAnnSrcSpanDP' @(Ghc.AnnList (Ghc.EpToken "hiding", [Ghc.EpToken ","])) $ Ghc.SameLine 0)
    { Ghc.anns = (Ghc.noAnn :: Ghc.AnnList (Ghc.EpToken "hiding", [Ghc.EpToken ","]))
      { Ghc.al_brackets = Ghc.ListParens (Ghc.EpTok EP.d1) (Ghc.EpTok EP.d0)
      , Ghc.al_rest = (Ghc.noAnn, [])
      }
    }
#elif MIN_VERSION_ghc(9,10,0)
  (noAnnSrcSpanDP' @Ghc.AnnList $ Ghc.SameLine 0)
    { Ghc.anns = (Ghc.noAnn :: Ghc.AnnList)
      { Ghc.al_open = Just $ Ghc.AddEpAnn Ghc.AnnOpenP EP.d1
      , Ghc.al_close = Just $ Ghc.AddEpAnn Ghc.AnnCloseP EP.d0
      }
    }
#else
  (noAnnSrcSpanDP' @Ghc.AnnList $ Ghc.SameLine 0)
    { Ghc.ann = Ghc.EpAnn
      { Ghc.anns = mempty
        { Ghc.al_open = Just $ Ghc.AddEpAnn Ghc.AnnOpenP EP.d0
        , Ghc.al_close = Just $ Ghc.AddEpAnn Ghc.AnnCloseP EP.d0
        }
      , Ghc.entry = Ghc.Anchor Ghc.placeholderRealSpan EP.m1
      , Ghc.comments = Ghc.emptyComments
      }
    }
#endif

nameAnn
  :: Bool
#if MIN_VERSION_ghc(9,10,0)
  -> Ghc.EpAnn Ghc.NameAnn
#else
  -> Ghc.SrcSpanAnn' (Ghc.EpAnn Ghc.NameAnn)
#endif
nameAnn False = Ghc.noSrcSpanA
nameAnn True =
#if MIN_VERSION_ghc(9,12,0)
  (Ghc.noAnn :: Ghc.EpAnn Ghc.NameAnn)
    { Ghc.anns = Ghc.NameAnn
      { Ghc.nann_adornment = nameParensAdornment
      , Ghc.nann_name = Ghc.noAnn
      , Ghc.nann_trailing  = []
      }
    }
#elif MIN_VERSION_ghc(9,10,0)
  (Ghc.noAnn :: Ghc.EpAnn Ghc.NameAnn)
    { Ghc.anns = Ghc.NameAnn
      { Ghc.nann_adornment = nameParensAdornment
      , Ghc.nann_name = Ghc.noAnn
      , Ghc.nann_trailing  = []
      , Ghc.nann_open = Ghc.noAnn
      , Ghc.nann_close = Ghc.noAnn
      }
    }
#else
  Ghc.SrcSpanAnn
    { Ghc.ann = Ghc.EpAnn
      { Ghc.anns = Ghc.NameAnn
        { Ghc.nann_adornment = nameParensAdornment
        , Ghc.nann_name = EP.d0
        , Ghc.nann_trailing  = []
        , Ghc.nann_open = EP.d0
        , Ghc.nann_close = EP.d0
        }
      , Ghc.entry = Ghc.Anchor Ghc.placeholderRealSpan UnchangedAnchor
      , Ghc.comments = Ghc.emptyComments
      }
    , Ghc.locA = Ghc.noSrcSpan
    }
#endif

importEpAnn :: Ghc.EpAnn Ghc.EpAnnImportDecl
#if MIN_VERSION_ghc(9,12,0)
importEpAnn = (Ghc.noAnn :: Ghc.EpAnn Ghc.EpAnnImportDecl)
  { Ghc.anns = Ghc.noAnn
    { Ghc.importDeclAnnImport = Ghc.EpTok Ghc.noAnn }
  }
#elif MIN_VERSION_ghc(9,10,0)
importEpAnn = Ghc.noAnn
#else
importEpAnn =
  Ghc.EpAnn
    { Ghc.entry = Ghc.Anchor Ghc.placeholderRealSpan EP.m0
    , Ghc.anns = Ghc.EpAnnImportDecl
        { Ghc.importDeclAnnImport = EP.d0
        , Ghc.importDeclAnnPragma = Nothing
        , Ghc.importDeclAnnSafe = Nothing
        , Ghc.importDeclAnnQualified = Nothing
        , Ghc.importDeclAnnPackage = Nothing
        , Ghc.importDeclAnnAs = Nothing
        }
    , Ghc.comments = Ghc.emptyComments
    }
#endif

hasTrailingComma :: SrcSpanAnnA -> Bool
#if MIN_VERSION_ghc(9,10,0)
hasTrailingComma = any (\case Ghc.AddCommaAnn{} -> True; _ -> False)
  . Ghc.lann_trailing . Ghc.anns
#else
hasTrailingComma x  =
  case Ghc.ann x of
    Ghc.EpAnnNotUsed -> False
    ann -> any (\case Ghc.AddCommaAnn{} -> True; _ -> False)
         . Ghc.lann_trailing $ Ghc.anns ann
#endif

pattern IEThingWith' :: XIEThingWith Ghc.GhcPs -> LIEWrappedName Ghc.GhcPs -> IEWildcard -> [LIEWrappedName Ghc.GhcPs] -> Ghc.IE Ghc.GhcPs
#if MIN_VERSION_ghc(9,10,0)
pattern IEThingWith' x name wc children = Ghc.IEThingWith x name wc children Nothing
#else
pattern IEThingWith' x name wc children = Ghc.IEThingWith x name wc children
#endif

pattern IEThingAbs' :: LIEWrappedName Ghc.GhcPs -> Ghc.IE Ghc.GhcPs
#if MIN_VERSION_ghc(9,10,0)
pattern IEThingAbs' name <- Ghc.IEThingAbs _ name _
#else
pattern IEThingAbs' name <- Ghc.IEThingAbs _ name
#endif

pattern IEVar' :: LIEWrappedName Ghc.GhcPs -> Ghc.IE Ghc.GhcPs
#if MIN_VERSION_ghc(9,10,0)
pattern IEVar' name = Ghc.IEVar Nothing name Nothing
#elif MIN_VERSION_ghc(9,8,0)
pattern IEVar' name = Ghc.IEVar Nothing name
#else
pattern IEVar' name = Ghc.IEVar Ghc.NoExtField name
#endif

pattern TcRnSolverReport' :: Ghc.SolverReportWithCtxt -> Ghc.TcRnMessage
#if MIN_VERSION_ghc(9,10,0)
pattern TcRnSolverReport' report <- Ghc.TcRnSolverReport report _
#else
pattern TcRnSolverReport' report <- Ghc.TcRnSolverReport report _ _
#endif
