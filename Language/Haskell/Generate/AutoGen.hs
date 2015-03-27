module Language.Haskell.Generate.AutoGen (saveModuleToFile) where

import Language.Haskell.Generate.AutoGen.Extra
import System.IO (writeFile)
import Language.Haskell.Generate.Monad (ModuleG, generateModule)

saveModuleToFile :: (ModuleG,String) -> FilePath -> IO ()
saveModuleToFile (m,nm) path = writeFile path (generateModule m nm)