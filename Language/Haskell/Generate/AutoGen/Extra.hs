module Language.Haskell.Generate.AutoGen.Extra where

import Control.Monad.Trans.Writer
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Pretty
import Language.Haskell.Generate.Monad
import qualified Data.Set as S

mkIntLit i = Lit (Int i)

mkQName s = UnQual (Ident s)

mkClassInstance :: String -> [String] -> [InstDecl] -> Decl
mkClassInstance cls names decls = InstDecl noLoc Nothing [] [] (mkQName cls) (map (TyCon . mkQName) names) decls

mkDataDecl nm pats constrs = DataDecl noLoc DataType [] (Ident nm) pats rhs' []
 where rhs' = map (\constr -> QualConDecl noLoc [] [] (ConDecl (Ident constr) [])) constrs

mkMatch nm pats exp = Match noLoc (Ident nm) pats Nothing (UnGuardedRhs exp) (BDecls [])

mkPragma :: [ValidPragma] -> ModulePragma
mkPragma ps = LanguagePragma noLoc (map (Ident . show) ps)

data ValidPragma = MultiParamTypeClasses | FunctionalDependencies
 deriving Show

-- TODO: Add as patch to haskell-generate
addDecl :: Decl -> ModuleM ()
addDecl inst = ModuleM $ do
 tell (S.empty, [inst])
 
addImport imp = ModuleM $ do
 tell (S.singleton (ModuleName imp), [])
 
generateModule' :: [String] -> [ModulePragma] -> ModuleG -> String -> String
generateModule' noquals pragmas = fmap prettyPrint . (runModuleM' noquals pragmas)
 
runModuleM' :: [String] -> [ModulePragma] -> ModuleG -> String -> Module
runModuleM' noquals pragmas (ModuleM act) name = 
  Module noLoc (ModuleName name) pragmas Nothing export (map (\(qual,md) -> ImportDecl noLoc md qual False False Nothing Nothing Nothing) imps'') decls
  where (export, (imps, decls)) = runWriter act
        imps' = map (\(ModuleName i) -> notElem i noquals) (S.toList imps)
        imps'' = zip imps' (S.toList imps)