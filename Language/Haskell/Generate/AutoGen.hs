module Language.Haskell.Generate.AutoGen (saveModuleToFile) where

import Language.Haskell.Generate.AutoGen.Extra
import Language.Haskell.Exts.Syntax (ModulePragma)
import System.IO (writeFile)
import Language.Haskell.Generate.Monad (ModuleG, generateModule)

saveModuleToFile :: [String] -> [ModulePragma] -> (ModuleG,String) -> FilePath -> IO ()
saveModuleToFile noquals pragma (m,nm) path = writeFile path (generateModule' noquals pragma m nm)