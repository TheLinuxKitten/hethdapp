{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}

--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

module Network.Web3.Dapp.Int.Internal
  ( IntN
  , intN
  , mkIntN
  , mkIntNT
  , UIntN
  , uintN
  , mkUIntN
  , mkUIntNT
  ) where

import Control.DeepSeq (NFData(..),deepseq)
import Data.Aeson
import Data.Bits
import Data.Char (intToDigit)
import Data.Proxy
import GHC.TypeLits (KnownNat, Nat, natVal)
import Language.Haskell.TH.Syntax
import Network.Web3.Dapp.EthABI.Types hiding (Type)

getBits :: (Bits b) => b -> [Bool]
getBits b = reverse
          $ foldr (\p r -> testBit b p : r) [] [0 .. bitSize b - 1]

bitStr :: (Bits b) => b -> String
bitStr = map (intToDigit . fromEnum) . getBits

mkMask :: Int -> [Int]
mkMask nBi
  | nBi <= 31 = [bit nBi - 1]
  | otherwise = 0xffffffff : mkMask (nBi - 31)

newtype IntN (n :: Nat) = IntN { unIntN :: Integer }
  deriving (Enum, Num, Ord, Real)

instance (KnownNat n) => Show (IntN n) where
  show = show . toInteger

sizeIntN :: (forall. KnownNat n) => IntN n -> Integer
sizeIntN = natVal

instance (forall. KnownNat n) => Eq (IntN n) where
  i1 == i2 = toInteger i1 == toInteger i2

instance (forall. KnownNat n) => Integral (IntN n) where
  (IntN i1) `quotRem` (IntN i2) = (IntN $ i1 `quot` i2, IntN $ i1 `rem` i2)
  toInteger (IntN i) = unIntN $ (intN i :: IntN n)

{-
instance (forall. KnownNat n) => Num (IntN n) where
  (IntN i1) + (IntN i2) = intN $ i1 + i2
  (IntN i1) * (IntN i2) = intN $ i1 * i2
  negate (IntN i) = intN $ negate i
  abs (IntN i) = intN $ abs i
  signum (IntN i) = intN $ signum i
  --fromInteger = IntN
  fromInteger = intN
-}
{-
instance (forall. KnownNat n) => Bits (IntN n) where
  bitSize = fromIntegral . sizeIntN
  bitSizeMaybe = Just . bitSize
  (IntN i1) .&. (IntN i2) = intN $ i1 .&. i2
  (IntN i1) .|. (IntN i2) = intN $ i1 .|. i2
  (IntN i1) `xor` (IntN i2) = intN $ i1 `xor` i2
  complement (IntN i) = intN $ complement i
  shift (IntN i) s = intN $ shift i s
  rotate (IntN i) s = intN $ rotate i s
  isSigned (IntN i) = isSigned i
  testBit (IntN i) n = testBit i n
  bit = intN . bit
  popCount (IntN i) = popCount i
-}
instance (forall. KnownNat n) => Bits (IntN n) where
  bitSize = fromIntegral . sizeIntN
  bitSizeMaybe = Just . bitSize
  (IntN i1) .&. (IntN i2) = IntN $ i1 .&. i2
  (IntN i1) .|. (IntN i2) = IntN $ i1 .|. i2
  (IntN i1) `xor` (IntN i2) = IntN $ i1 `xor` i2
  complement (IntN i) = IntN $ complement i
  shift (IntN i) s = IntN $ shift i s
  rotate (IntN i) s = IntN $ rotate i s
  isSigned (IntN i) = isSigned i
  testBit (IntN i) n = testBit i n
  bit = IntN . bit
  popCount (IntN i) = popCount i

instance (forall. KnownNat n) => FiniteBits (IntN n) where
  finiteBitSize = bitSize

intN :: forall n. KnownNat n => Integer -> IntN n
intN a = IntN $ val a
  where
    nBits :: KnownNat n => Integer
    nBits = natVal (Proxy :: Proxy n)
    minVal = negate $ 2 ^ (nBits - 1)
    maxVal = 2 ^ (nBits - 1) - 1
    val a
      | a < minVal = narrowN a
      | a > maxVal = narrowN a
      | otherwise = a
    narrowN a = let v = a .&. maxVal
                in if testBit a (fromIntegral nBits - 1)
                     then negate $ maxVal - v + 1
                     else v

mkIntType :: Bool -> Int -> Q [Dec]
mkIntType sg nBi = do
  let tyName = mkName $ (if sg then "Sint" else "Uint") ++ show nBi
  let nT = LitT $ NumTyLit $ toInteger nBi
  intnT <- [t| IntN |]
  uintnT <- [t| UIntN |]
  let tyD = TySynD tyName [] (AppT (if sg then intnT else uintnT) nT)
  integerT <- [t| Integer |]
  let funName = mkName $ (if sg then "int" else "uint") ++ show nBi
  let funSigD = SigD funName (AppT (AppT ArrowT integerT) (ConT tyName))
  let intnE = VarE $ mkName (if sg then "intN" else "uintN")
  let funD = FunD funName [Clause [] (NormalB intnE) []]
  return [tyD {-, funSigD, funD-}]

mkIntTypeT :: Bool -> Int -> Q Type
mkIntTypeT sg nBi = return $ ConT $ mkName
                  $ (if sg then "Sint" else "Uint") ++ show nBi

mkIntN = mkIntType True
mkIntNT = mkIntTypeT True

instance (KnownNat n) => AbiValueEncoding (IntN n) where
  toAbiValue = toAbiValue . toInteger
  fromAbiValue (AVDec i) = Right $ intN i
  fromAbiValue av = fromAbiErr "IntN" av

instance (KnownNat n) => NFData (IntN n) where
  rnf (IntN i) = i `deepseq` ()

instance (KnownNat n) => FromJSON (IntN n) where
  parseJSON v = IntN <$> parseJSON v

newtype UIntN (n :: Nat) = UIntN { unUIntN :: Integer }
  deriving (Enum, Num, Ord, Real)

instance (KnownNat n) => Show (UIntN n) where
  show = show . toInteger

sizeUIntN :: (forall. KnownNat n) => UIntN n -> Integer
sizeUIntN = natVal

instance (forall. KnownNat n) => Eq (UIntN n) where
  i1 == i2 = toInteger i1 == toInteger i2

instance (forall. KnownNat n) => Integral (UIntN n) where
  (UIntN i1) `quotRem` (UIntN i2) = (UIntN $ i1 `quot` i2, UIntN $ i1 `rem` i2)
  toInteger (UIntN i) = unUIntN $ (uintN i :: UIntN n)

{-
instance (forall. KnownNat n) => Num (UIntN n) where
  (UIntN i1) + (UIntN i2) = uintN $ i1 + i2
  (UIntN i1) * (UIntN i2) = uintN $ i1 * i2
  negate (UIntN i) = uintN $ negate i
  abs (UIntN i) = uintN $ abs i
  signum (UIntN 0) = 0
  signum _ = 1
  fromInteger = uintN
-}
{-
instance (forall. KnownNat n) => Bits (UIntN n) where
  bitSize = fromIntegral . sizeUIntN
  bitSizeMaybe = Just . bitSize
  (UIntN i1) .&. (UIntN i2) = uintN $ i1 .&. i2
  (UIntN i1) .|. (UIntN i2) = uintN $ i1 .|. i2
  (UIntN i1) `xor` (UIntN i2) = uintN $ i1 `xor` i2
  complement (UIntN i) = uintN $ complement i
  shift (UIntN i) s = uintN $ shift i s
  rotate (UIntN i) s = uintN $ rotate i s
  isSigned _ = False
  testBit (UIntN i) n = testBit i n
  bit = uintN . bit
  popCount (UIntN i) = popCount i
-}
instance (forall. KnownNat n) => Bits (UIntN n) where
  bitSize = fromIntegral . sizeUIntN
  bitSizeMaybe = Just . bitSize
  (UIntN i1) .&. (UIntN i2) = UIntN $ i1 .&. i2
  (UIntN i1) .|. (UIntN i2) = UIntN $ i1 .|. i2
  (UIntN i1) `xor` (UIntN i2) = UIntN $ i1 `xor` i2
  complement (UIntN i) = UIntN $ complement i
  shift (UIntN i) s = UIntN $ shift i s
  rotate (UIntN i) s = UIntN $ rotate i s
  isSigned _ = False
  testBit (UIntN i) n = testBit i n
  bit = UIntN . bit
  popCount (UIntN i) = popCount i

instance (forall. KnownNat n) => FiniteBits (UIntN n) where
  finiteBitSize = bitSize

uintN :: forall n. KnownNat n => Integer -> UIntN n
uintN a = UIntN $ a .&. maskVal -- val a `mod` (modVal a)
    where
    nBits :: KnownNat n => Integer
    nBits = natVal (Proxy :: Proxy n)
    maskVal = bit (fromIntegral nBits) - 1
    {-
    maxVal nBi = 2 ^ nBi - 1
    modVal v = maxVal nBits + 1
    val v = if v < 0 then maxVal nBits + 1 + v else v
    -}

mkUIntN = mkIntType False
mkUIntNT = mkIntTypeT False

instance (KnownNat n) => AbiValueEncoding (UIntN n) where
  toAbiValue = toAbiValue . toInteger
  fromAbiValue (AVDec i) = Right $ uintN i
  fromAbiValue av = fromAbiErr "UIntN" av

instance (KnownNat n) => NFData (UIntN n) where
  rnf (UIntN i) = i `deepseq` ()

instance (KnownNat n) => FromJSON (UIntN n) where
  parseJSON v = UIntN <$> parseJSON v

