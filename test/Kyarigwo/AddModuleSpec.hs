module Kyarigwo.AddModuleSpec(spec) where

import ClassyPrelude
import Test.Hspec
import Kyarigwo.AddModule
import qualified Data.List.NonEmpty as N
import qualified Data.Text as T

spec :: Spec
spec = do
  splitModuleNameSpec
  regularExprMatchSpec

splitModuleNameSpec :: Spec
splitModuleNameSpec = describe "splitModuleName" $
  it "splits Kyarigwo.AddModule.Core" $
    splitModuleName (N.fromList (T.split (== '.')   "Kyarigwo.AddModule.Core")) `shouldBe`
    ModuleNameComponents "Core"
     "Kyarigwo/AddModule"
     "Kyarigwo.AddModule"
     "Kyarigwo.AddModule.Core"

regularExprMatchSpec :: Spec
regularExprMatchSpec = describe "matchFileStart" $ do
  it "should match {-# START_FILE Simple #-}" $
      matchFileStart "{-# START_FILE Simple #-}" `shouldBe` Just "Simple"
  it "should match {-# START_FILE src/Kyarigwo/{{fileSystemPath}}/{{baseName}} #-}" $
      matchFileStart "{-# START_FILE src/Kyarigwo/{{fileSystemPath}}/{{baseName}} #-}"
         `shouldBe` Just "src/Kyarigwo/{{fileSystemPath}}/{{baseName}}"
  it "should not match 'anything else'" $
      matchFileStart "anything else"
         `shouldBe` Nothing
  it "should match with spaces '  {-#   START_FILE  Simple #-}  '" $
      matchFileStart "  {-#   START_FILE  Simple #-}  " `shouldBe` Just "Simple"


