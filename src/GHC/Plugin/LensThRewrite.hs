{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE BangPatterns    #-}
--------------------------------------------------------------------------------
-- |
-- Module      : GHC.Plugin.LensThRewrite
-- Copyright   : (c) 2020 David Johnson
-- License     : All Rights Reserved
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
-- GHC Plugin to rewrite makeLenses call into pure functions.
--
--------------------------------------------------------------------------------
module GHC.Plugin.LensThRewrite ( plugin, rewriteModule ) where

import Control.Arrow
import Control.Lens
import Data.Function                           (on)
import Data.List

import CoreSyn
import GhcPlugins
import HsDecls
import HsDumpAst
import HsExtension
import HsSyn
import OccName
import RdrName
import TcEvidence
import Var

-- | Lens rewrite plugin.
plugin :: Plugin
plugin
  = defaultPlugin
  { parsedResultAction = \_ _ -> rewriteMakeLenses
  , pluginRecompile = purePlugin
  }

rewriteModule
   :: HsModule GhcPs
   -> HsModule GhcPs
rewriteModule module'
  = module' & decls %~ concatMap (modifyDecls module')

rewriteMakeLenses
   :: HsParsedModule
   -> Hsc HsParsedModule
rewriteMakeLenses parsed
  = pure
  $ parsed
  & moduleDecls
  %~ concatMap (modifyDecls (parsed ^. (hsMod . located)))

parsedModule :: Lens' HsParsedModule (Located (HsModule GhcPs))
parsedModule = lens hpm_module $ \r f -> r { hpm_module = f }

moduleDecls :: Lens' HsParsedModule [LHsDecl GhcPs]
moduleDecls = hsMod . located . decls

hsMod :: Lens' HsParsedModule (Located (HsModule GhcPs))
hsMod = lens hpm_module $ \f r -> f { hpm_module = r }

located :: Lens' (Located a) a
located = lens getter setter
  where
    getter (L _ r) = r
    setter (L x _) y = L x y

decls :: Lens' (HsModule a) [LHsDecl a]
decls = lens hsmodDecls $ \f r -> f { hsmodDecls = r }

modifyDecls :: HsModule GhcPs -> LHsDecl GhcPs -> [LHsDecl GhcPs]
modifyDecls m (L x decl) | not (isMakeLensesThSplice decl) = pure (L x decl)
modifyDecls m (L x decl) = emptyL <$> toDecls decl
  where
    toDecls :: HsDecl GhcPs -> [HsDecl GhcPs]
    toDecls decl = concat $ genDecls (getDecls m) =<< getMakeLensesSplices decl
        where
          genDecls decls type'
            | Just fields <- lookup type' decls = genLensCall type' <$> fields
            | otherwise = []

isMakeLensesThSplice :: HsDecl GhcPs -> Bool
isMakeLensesThSplice (SpliceD _ (SpliceDecl _ (L _ splice) _)) =
  case splice of
    HsUntypedSplice _ _ _ (L _ expr) ->
      case expr of
        HsApp _ (L _ l) (L _ r) ->
          case l of
            HsVar NoExt (L _ (Unqual (occNameString -> "makeLenses"))) ->
              True
            _ -> False
        _ -> False
    _ -> False
isMakeLensesThSplice _ = False

getMakeLensesSplices :: HsDecl GhcPs -> [String]
getMakeLensesSplices (SpliceD _ (SpliceDecl _ (L _ splice) _)) =
  case splice of
    HsUntypedSplice _ _ _ (L _ expr) ->
      case expr of
        HsApp _ (L _ l) (L _ r) ->
          case l of
            HsVar NoExt (L _ (Unqual (occNameString -> "makeLenses"))) ->
              case r of
                HsBracket NoExt (VarBr NoExt False (Unqual (occNameString -> typ))) ->
                  [typ]
                _ -> []
            _ -> []
        _ -> []
    _ -> []
getMakeLensesSplices _ = []

mkVar :: String -> HsExpr GhcPs
mkVar x = HsVar NoExt (mkName x)

type FieldName = String
type TypeName = String

genSigD
  :: FieldName
  -> TypeName
  -- ^ Inner type, i.e. "Person" in Lens' Person Int
  -> HsType GhcPs
  -- ^ Outer type, i.e. "Int" in Lens' Person Int
  -> HsDecl GhcPs
genSigD fieldName innerType outerType =
  SigD NoExt (TypeSig NoExt [ mkName fieldName ] hsWc)
    where
      hsWc = HsWC NoExt hsIb
      hsIb = HsIB NoExt (emptyL result)
      result = tyVarLens `appTy` tyVarTypeInner `appTy` outerType
      tyVarTypeInner = tyVar innerType

appTy :: HsType GhcPs -> HsType GhcPs -> HsType GhcPs
appTy = HsAppTy NoExt `on` emptyL

tyVarLens :: HsType GhcPs
tyVarLens = tyC "Lens'"

tyVar :: String -> HsType GhcPs
tyVar s = HsTyVar NoExt NotPromoted (mkTyVarName s)

tyC :: String -> HsType GhcPs
tyC s = HsTyVar NoExt NotPromoted (mkTyCName s)

getDecls :: HsModule GhcPs -> [(TypeName, [(FieldName, HsType GhcPs)])]
getDecls mod = concatMap go $ fmap (^. located) (hsmodDecls mod)
  where
    go :: HsDecl GhcPs -> [(String, [(String,HsType GhcPs)])]
    go (TyClD NoExt d) = [(getDeclTypeName &&& getFieldAndTypeName) d]
    go _ = []

mkName :: String -> Located RdrName
mkName = emptyL . mkRdrUnqual . mkOccName OccName.varName

mkTyVarName :: String -> Located RdrName
mkTyVarName = emptyL . mkRdrUnqual . mkOccName OccName.tcName

mkTyCName :: String -> Located RdrName
mkTyCName = emptyL . mkRdrUnqual . mkOccName OccName.tcName


-- | Extract existing type information from a Type or class Decl
getDeclTypeName :: TyClDecl GhcPs -> String
getDeclTypeName DataDecl {..} =
  case tcdLName ^. located of
    Unqual (occNameString -> s) -> s
getDeclTypeName _ = mempty

-- | Extract field name information from a record
getFieldAndTypeName :: TyClDecl GhcPs -> [(String,HsType GhcPs)]
getFieldAndTypeName DataDecl {..} = concat . concat $
  dd_cons tcdDataDefn <&> \(L _ ConDeclH98 {..}) ->
    case con_args of
      RecCon (L _ xs) ->
        xs <&> \(L _ ConDeclField{..}) ->
          case cd_fld_names of
            [ L _ FieldOcc {..} ] ->
              case rdrNameFieldOcc of
                L _ (Unqual fieldName) ->
                  pure (occNameString fieldName, cd_fld_type ^. located)
                _ -> []
            _ -> []
      _ -> []
getFieldAndTypeName _ = []

genLensCall
  :: String
  -> (String, HsType GhcPs)
  -> [HsDecl GhcPs]
genLensCall lensInnerType (fieldName, fieldType) =
  [ genSigD lensName lensInnerType fieldType, valD (funBind lensName mg) ]
  where
    lensName = drop 1 fieldName
    mg =
      matchGroup
      [ match (funRhs lensName) $ grhss
        [ grhs $
          (hsVar "lens" `hsApp` hsVar fieldName)
            `hsApp`
               (hsPar
                 $ hsLam
                 $ matchGroup
#if MIN_VERSION_base (4,13,0)
                 [ lambdaMatch [ varPat "r", varPat "f" ] $
#else
                 [ lambdaMatch [ emptyL (varPat "r"), emptyL (varPat "f") ] $
#endif
                   grhss
                   [ grhs $ recordUpd (hsVar "r")
                     [ hsRecUpdField fieldName (hsVar "f")
                     ]
                   ]
                 ]
               )
        ]
      ]

valD :: HsBind GhcPs -> HsDecl GhcPs
valD = ValD NoExt

funBind :: String -> MatchGroup GhcPs (LHsExpr GhcPs) -> HsBind GhcPs
funBind s mg = FunBind NoExt (mkName s) mg WpHole []

matchGroup :: [Match GhcPs (LHsExpr GhcPs)] -> MatchGroup GhcPs (LHsExpr GhcPs)
matchGroup xs = MG NoExt (emptyL (fmap emptyL xs)) FromSource

match
  :: HsMatchContext (NameOrRdrName (IdP GhcPs)) -- see funRhs
  -> GRHSs GhcPs (LHsExpr GhcPs)
  -> Match GhcPs (LHsExpr GhcPs)
match x y = Match NoExt x [] y

lambdaMatch
  :: [LPat GhcPs]
  -> GRHSs GhcPs (LHsExpr GhcPs)
  -> Match GhcPs (LHsExpr GhcPs)
lambdaMatch xs y = Match NoExt LambdaExpr xs y

funRhs :: String -> HsMatchContext (NameOrRdrName (IdP GhcPs))
funRhs x = FunRhs (mkName x) Prefix NoSrcStrict

emptyL :: e -> GenLocated SrcSpan e
emptyL = L noSrcSpan

grhss :: [GRHS GhcPs (LHsExpr GhcPs)] -> GRHSs GhcPs (LHsExpr GhcPs)
grhss xs = GRHSs NoExt (fmap emptyL xs) (emptyL (EmptyLocalBinds NoExt))

grhs :: HsExpr GhcPs -> GRHS GhcPs (LHsExpr GhcPs)
grhs = GRHS NoExt [] . emptyL

hsApp :: HsExpr GhcPs -> HsExpr GhcPs -> HsExpr GhcPs
hsApp l r = HsApp NoExt (emptyL l) (emptyL r)

hsVar :: String -> HsExpr GhcPs
hsVar = HsVar NoExt . mkName

hsPar :: HsExpr GhcPs -> HsExpr GhcPs
hsPar = HsPar NoExt . emptyL

hsLam :: MatchGroup GhcPs (LHsExpr GhcPs) -> HsExpr GhcPs
hsLam = HsLam NoExt

recordUpd :: HsExpr GhcPs -> [HsRecUpdField GhcPs] -> HsExpr GhcPs
recordUpd e fs = RecordUpd NoExt (emptyL e) (emptyL <$> fs)

hsRecUpdField
  :: String
  -> HsExpr GhcPs
  -> HsRecUpdField GhcPs
hsRecUpdField s e = HsRecField (emptyL (ambig s)) (emptyL e) False
  where
    ambig :: String -> AmbiguousFieldOcc GhcPs
    ambig s = Unambiguous NoExt (mkName s)

varPat :: String -> Pat GhcPs
varPat = VarPat NoExt . mkName
