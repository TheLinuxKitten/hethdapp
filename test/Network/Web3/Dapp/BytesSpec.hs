{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Web3.Dapp.BytesSpec (main, spec) where

import Data.Either (isLeft, isRight)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Web3.Dapp.Bytes
import Network.Web3.Dapp.EthABI.Types
import Network.Web3.Dapp.Int
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Bytes" $ do
  it "Conversiones BytesString" $ do
    lengthN (zeroN :: Bytes27) `shouldBe` 27
    lengthN (zeroN :: Bytes1) `shouldBe` 1
    bytesN (bytes5 "1234567890") `shouldBe` "67890"
    bytesN (bytes5 "123") `shouldBe` "123\0\0"
    bytesN (bytes5 "12345") `shouldBe` "12345"
  it "Conversiones fromString" $ do
    bytesN ("123" :: Bytes5) `shouldBe` "123\0\0"
    bytesN ("12345" :: Bytes5) `shouldBe` "12345"
    bytesN ("1234567890" :: Bytes5) `shouldBe` "67890"
  it "Conversiones AbiValue" $ do
    toAbiValue (bytes5 "12345") `shouldBe` AVBytes "12345"
    (fromAbiValue (AVDec 5) :: Either Text Bytes5) `shouldSatisfy` isLeft
    (fromAbiValue (AVBytes "12") :: Either Text Bytes2) `shouldSatisfy` isRight
  it "Conversiones Integer" $ do
    toInteger (toUIntN ("\xff\xff" :: Bytes1) :: Uint16) `shouldBe` 0xff
    toInteger (toUIntN ("\xff\xff" :: Bytes2) :: Uint16) `shouldBe` 0xffff
    bytesN (fromUIntN (toUIntN ("\xff\xff" :: Bytes1) :: Uint16) :: Bytes2)
      `shouldBe` "\xff\0"
    bytesN (fromUIntN (toUIntN ("\xff\xff" :: Bytes1) :: Uint16) :: Bytes1)
      `shouldBe` "\xff"
    bytesN (fromUIntN (toUIntN ("" :: Bytes1) :: Uint16) :: Bytes2)
      `shouldBe` "\0\0"
    bytesN (fromUIntN (toUIntN ("" :: Bytes1) :: Uint16) :: Bytes1)
      `shouldBe` "\0"
    bytesN (("" :: Bytes2) + ("Ho" :: Bytes2)) `shouldBe` "Ho"
    bytesN (("\xff\xff" :: Bytes2) + ("Ho" :: Bytes2)) `shouldBe` "Hn"
    bytesN (("\0\0\x1" :: Bytes3) * ("Hol" :: Bytes3)) `shouldBe` "Hol"

