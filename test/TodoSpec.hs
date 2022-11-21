module TodoSpec where

import Test.Hspec

import Todo
import Data.Foldable

spec :: Spec
spec = describe "Simple test" $ do

     it "" $
        True `shouldBe` True
