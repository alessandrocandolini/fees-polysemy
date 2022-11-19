module TodoSpec where

import Test.Hspec

import Todo
import Data.Foldable

spec :: Spec
spec = describe "Simple test" $ do

     it "count bracket" $
        (fold input) `shouldBe` 6 where
          input :: [AdditiveInteger]
          input = [1,2,3]
