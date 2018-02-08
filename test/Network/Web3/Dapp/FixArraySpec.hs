{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Web3.Dapp.FixArraySpec (main, spec) where

import Data.Either (isLeft, isRight)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Web3.Dapp.FixArray
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "FixArray" $ do
  it "FixArray y listas" $ do
    fromFixArray ((NilL|>1|>2|>3|>4|>5) :: FixArray 5 Int)
      `shouldBe` [1,2,3,4,5::Int]
    fromFixArray (toFixArray' [1,2,3,4,5] :: FixArray 5 Int)
      `shouldBe` [1,2,3,4,5::Int]
    (toFixArray [1,2,3] :: Either Text (FixArray 4 Int))
      `shouldSatisfy` isLeft
    (toFixArray [1,2,3] :: Either Text (FixArray 3 Int))
      `shouldSatisfy` isRight

