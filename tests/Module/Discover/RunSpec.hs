module Module.Discover.RunSpec
  ( spec
  ) where

import Prelude

import Module.Discover.Run
import Test.Hspec

spec :: Spec
spec = do
  describe "run" $ do
    it "matches expected modules" $ do
      run
        (pure . show)
        Nothing
        ["example-dir/Some/Module/", "[unused]", "/tmp/foo.txt"]
      (read <$> readFile "/tmp/foo.txt")
        `shouldReturn` [ Module
                          { moduleName = "Name"
                          , moduleFullName = "Some.Module.Name"
                          , moduleFilePath = "example-dir/Some/Module/Name.hs"
                          }
                       , Module
                          { moduleName = "Name1"
                          , moduleFullName = "Some.Module.Path.Name1"
                          , moduleFilePath =
                              "example-dir/Some/Module/Path/Name1.hs"
                          }
                       , Module
                          { moduleName = "Name2"
                          , moduleFullName = "Some.Module.Path.Name2"
                          , moduleFilePath =
                              "example-dir/Some/Module/Path/Name2.hs"
                          }
                       , Module
                          { moduleName = "Name3"
                          , moduleFullName = "Some.Module.Path.Name3"
                          , moduleFilePath =
                              "example-dir/Some/Module/Path/Name3.hs"
                          }
                       ]
