module MonoidalParsingSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck


import MonoidalParsing

spec :: Spec
spec = describe "Monoidal parsing examples" $ do

     prop "mempty is the left neutral element" $
      \a b -> (a >= 0 && b >= 0) ==> Balance a b <> mempty == Balance a b 

     prop "mempty is the right neutral element" $
      \a b -> (a >= 0 && b >= 0) ==> mempty <> Balance a b == Balance a b 

     it "balance ( + )( + )" $
        Balance 0 1 <> Balance 1 1 <> Balance 1 0 `shouldBe` Balance 0 0
     it "balance ( + )( + ) 5 times" $
        Balance 0 5 <> Balance 5 5 <> Balance 5 0 `shouldBe` Balance 0 0
     it "()" $
        balance "()" `shouldBe` Balance 0 0

     it ")(" $
        balance ")(" `shouldBe` Balance 1 1

     it "))" $
        balance "))" `shouldBe` Balance 2 0

     it "((" $
        balance "((" `shouldBe` Balance 0 2

     it "a(b)c" $
        balance "a(b)c" `shouldBe` Balance 0 0

     it "a)b(c" $
        balance "a)b(c" `shouldBe` Balance 1 1

     it "a)b)c" $
        balance "a)b)c" `shouldBe` Balance 2 0

     it "a(b(c" $
        balance "a(b(c" `shouldBe` Balance 0 2

     it "(()" $
        balance "(()" `shouldBe` Balance 0 1

     it "(a(b))c(d)e" $
        balance "(a(b))c(d)e" `shouldBe` Balance 0 0

     it "()((())()(()))(())" $
        balance "()((())()(()))(())" `shouldBe` Balance 0 0
     
     it "count bracket" $
        (fold input) `shouldBe` 6 where
          input :: [AdditiveInteger]
          input = [1,2,3]
