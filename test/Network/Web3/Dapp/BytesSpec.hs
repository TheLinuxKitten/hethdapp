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
      `shouldBe` "\0\xff"
    bytesN (fromUIntN (toUIntN ("\xff\xff" :: Bytes1) :: Uint16) :: Bytes1)
      `shouldBe` "\xff"
    bytesN (fromUIntN (toUIntN ("" :: Bytes1) :: Uint16) :: Bytes2)
      `shouldBe` "\0\0"
    bytesN (fromUIntN (toUIntN ("" :: Bytes1) :: Uint16) :: Bytes1)
      `shouldBe` "\0"
    bytesN (("" :: Bytes2) + ("Ho" :: Bytes2)) `shouldBe` "Ho"
    bytesN (("\xff\xff" :: Bytes2) + ("Ho" :: Bytes2)) `shouldBe` "Hn"
    bytesN (("\0\0\x1" :: Bytes3) * ("Hol" :: Bytes3)) `shouldBe` "Hol"
    let u8s = [401,327,325,297,257,256,255,254,140,139,138,137,136,135,134,133,132,131,130,129,128,127,126,50,2,1,0,-0,-1,-2,-15,-16,-48,-119,-122,-124,-125,-126,-127,-128,-129,-130,-143,-159,-199,-220,-224,-225,-253,-254,-255,-256,-257,-258,-283,-290,-320,-345,-390::UIntN 8]
    map (toInteger . (fromInteger :: Integer -> Bytes2) . toInteger) u8s `shouldBe`
      [145,71,69,41,1,0,255,254,140,139,138,137,136,135,134,133,132,131,130,129,128,127,126,50,2,1,0,0,255,254,241,240,208,137,134,132,131,130,129,128,127,126,113,97,57,36,32,31,3,2,1,0,255,254,229,222,192,167,122]
    toInteger (sum $ map ((fromInteger :: Integer -> Bytes2) . toInteger) u8s)
      `shouldBe` 7069
    map (toInteger . complement . (fromInteger :: Integer -> Bytes1) . toInteger) u8s `shouldBe` [110,184,186,214,254,255,0,1,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,205,253,254,255,255,0,1,14,15,47,118,121,123,124,125,126,127,128,129,142,158,198,219,223,224,252,253,254,255,0,1,26,33,63,88,133]
    map (toInteger . complement . (fromInteger :: Integer -> Bytes2) . toInteger) u8s `shouldBe` [65390,65464,65466,65494,65534,65535,65280,65281,65395,65396,65397,65398,65399,65400,65401,65402,65403,65404,65405,65406,65407,65408,65409,65485,65533,65534,65535,65535,65280,65281,65294,65295,65327,65398,65401,65403,65404,65405,65406,65407,65408,65409,65422,65438,65478,65499,65503,65504,65532,65533,65534,65535,65280,65281,65306,65313,65343,65368,65413]
    map (toInteger . complement . (fromUIntN :: Uint8 -> Bytes2)) u8s `shouldBe` [65390,65464,65466,65494,65534,65535,65280,65281,65395,65396,65397,65398,65399,65400,65401,65402,65403,65404,65405,65406,65407,65408,65409,65485,65533,65534,65535,65535,65280,65281,65294,65295,65327,65398,65401,65403,65404,65405,65406,65407,65408,65409,65422,65438,65478,65499,65503,65504,65532,65533,65534,65535,65280,65281,65306,65313,65343,65368,65413]
    map ((toUIntN :: Bytes2 -> Uint32) . complement . (fromUIntN :: Uint8 -> Bytes2)) u8s `shouldBe` [65390,65464,65466,65494,65534,65535,65280,65281,65395,65396,65397,65398,65399,65400,65401,65402,65403,65404,65405,65406,65407,65408,65409,65485,65533,65534,65535,65535,65280,65281,65294,65295,65327,65398,65401,65403,65404,65405,65406,65407,65408,65409,65422,65438,65478,65499,65503,65504,65532,65533,65534,65535,65280,65281,65306,65313,65343,65368,65413]
    map ((toUIntN :: Bytes4 -> Uint32) . complement . (fromUIntN :: Uint8 -> Bytes4)) u8s `shouldBe` [4294967150,4294967224,4294967226,4294967254,4294967294,4294967295,4294967040,4294967041,4294967155,4294967156,4294967157,4294967158,4294967159,4294967160,4294967161,4294967162,4294967163,4294967164,4294967165,4294967166,4294967167,4294967168,4294967169,4294967245,4294967293,4294967294,4294967295,4294967295,4294967040,4294967041,4294967054,4294967055,4294967087,4294967158,4294967161,4294967163,4294967164,4294967165,4294967166,4294967167,4294967168,4294967169,4294967182,4294967198,4294967238,4294967259,4294967263,4294967264,4294967292,4294967293,4294967294,4294967295,4294967040,4294967041,4294967066,4294967073,4294967103,4294967128,4294967173]
    map ((toUIntN :: Bytes4 -> Uint8) . complement . (fromUIntN :: Uint8 -> Bytes4) . toInteger) u8s `shouldBe` [110,184,186,214,254,255,0,1,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,205,253,254,255,255,0,1,14,15,47,118,121,123,124,125,126,127,128,129,142,158,198,219,223,224,252,253,254,255,0,1,26,33,63,88,133]
    sum (map (popCount . complement . (fromUIntN :: Uint8 -> Bytes4) . toInteger) u8s) `shouldBe` 1689

