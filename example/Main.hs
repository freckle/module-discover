module Main
  ( main
  ) where

import Prelude

import Module.Discover.Run (Module (..), run)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= run (pure . make) Nothing

make :: [Module] -> String
make modules =
  unlines $
    ["module Main (main) where", ""]
      <> (toQualifiedImport <$> modules)
      <> ["", "main :: IO ()", "main = do"]
      <> (toMainCall <$> modules)

toQualifiedImport :: Module -> String
toQualifiedImport Module {..} =
  "import qualified " <> moduleFullName <> " as " <> moduleName

toMainCall :: Module -> String
toMainCall Module {..} = "  " <> moduleName <> ".main"
