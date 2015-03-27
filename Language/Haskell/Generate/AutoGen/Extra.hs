module Language.Haskell.Generate.AutoGen.Extra where

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc

mkIntLit i = Lit (Int i)

mkQName s = UnQual (Symbol s)

mkClassInstance :: String -> [String] -> [InstDecl] -> Decl
mkClassInstance cls names decls = InstDecl noLoc Nothing [] [] (mkQName cls) (map (TyCon . mkQName) names) decls

mkMatch name exp = Match noLoc (Symbol name) [] Nothing (UnGuardedRhs exp) (BDecls [])