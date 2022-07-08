module Module.Discover.Run
  ( run
  , Module(..)
  ) where

import Prelude

import Data.Char (isUpper)
import Data.List.Extra (replace)
import System.Exit (exitFailure)
import System.FilePath
  ( dropExtension
  , joinPath
  , pathSeparator
  , splitPath
  , takeDirectory
  , takeFileName
  , (</>)
  )
import System.FilePattern.Directory (getDirectoryFiles)
import System.IO (hPutStrLn, stderr)

run
  :: ([Module] -> IO String) -- ^ Output function
  -> [String] -- ^ Processor arguments
  -> IO ()
run output args = do
  case args of
    src : _ : dst : filePattern : _args -> do
      let srcDir = takeDirectory src
      modules <- fmap (toModule srcDir)
        <$> getDirectoryFiles srcDir [filePattern]
      out <- output modules
      writeFile dst out
    _ -> do
      hPutStrLn stderr "unexpected arguments"
      exitFailure

toModule :: FilePath -> FilePath -> Module
toModule srcDir filePath = Module
  { moduleName = dropExtension $ takeFileName filePath
  , moduleFullName = toModuleFullName srcDir filePath
  , moduleFilePath = srcDir </> filePath
  }

toModuleFullName :: FilePath -> FilePath -> String
toModuleFullName srcDir filePath =
  replace [pathSeparator] "."
    $ dropExtension
    $ joinPath
    $ reverse
    $ takeWhile (isUpper . head)
    $ reverse
    $ splitPath
    $ srcDir
    </> filePath

data Module = Module
  { moduleName :: String
  , moduleFullName :: String
  , moduleFilePath :: FilePath
  }
  deriving stock (Eq, Ord, Show)
