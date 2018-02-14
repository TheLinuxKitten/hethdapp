{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
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

module Network.Web3.Dapp.Bytes.Internal
  ( BytesN
  , bytesN
  , zeroN
  , updateN
  , lengthN
  , mkBytesN
--  , mkAbiValueEncodingN
--  , mkIsStringN
  , mkBytesNT
  , fromUIntN
  , toUIntN
  ) where

import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import Data.Monoid ((<>))
import Data.Proxy
import Data.Ratio
import qualified Data.Text as T
import GHC.Exts
import GHC.TypeLits (KnownNat, Nat, natVal)
import Language.Haskell.TH.Syntax
import Network.Web3.Dapp.EthABI.Types hiding (Type)
import Network.Web3.Dapp.Int

newtype BytesN (n :: Nat) = BytesN { unBytesN :: BS.ByteString }
                          deriving (Eq, Ord, Read)

bytesN :: (KnownNat n) => BytesN n -> BS.ByteString
bytesN = unBytesN

instance (KnownNat n) => Show (BytesN n) where
  show = show . ("0x"<>) . B16.encode . bytesN

lengthN :: (KnownNat n) => BytesN n -> Int
lengthN = fromIntegral . natVal

emptyN :: (KnownNat n) => BytesN n
emptyN = BytesN BS.empty

zeroN :: (KnownNat n) => BytesN n
zeroN = updateN emptyN BS.empty

updateN :: (KnownNat n) => BytesN n -> BS.ByteString -> BytesN n
updateN bn bs =
  let nBy = lengthN bn
      lenBs = BS.length bs
  in BytesN $ if lenBs > nBy
               then BS.drop (lenBs - nBy) bs
               else if lenBs < nBy
                     then bs <> BS.pack (replicate (nBy-lenBs) 0)
                     else bs

mkBytesN :: Int -> Q [Dec]
mkBytesN nBy = do
  let tyName = mkName ("Bytes" ++ show nBy)
  bytesT <- [t| BytesN |]
  let nT = LitT $ NumTyLit $ toInteger nBy
  let typeD = TySynD tyName [] (AppT bytesT nT)
  let funName = mkName ("bytes" ++ show nBy)
  bsT <- [t| BS.ByteString |]
  let funSigD = SigD funName (AppT (AppT ArrowT bsT) (ConT tyName))
  let updateNE = VarE $ mkName "updateN"
  let zeroNE = VarE $ mkName "zeroN"
  let bodyE = AppE updateNE (SigE zeroNE (ConT tyName))
  let funD = FunD funName [Clause [] (NormalB bodyE) []]
  let funZName = mkName ("zeroBytes" ++ show nBy)
  let funZSigD = SigD funZName (ConT tyName)
  let funZD = FunD funZName [Clause [] (NormalB zeroNE) []]
  return [typeD, funSigD, funD, funZSigD, funZD]

instance (KnownNat n) => AbiValueEncoding (BytesN n) where
  toAbiValue = toAbiValue . unBytesN
  fromAbiValue (AVBytes bs) = Right $ updateN emptyN bs
  fromAbiValue av = fromAbiErr "BytesN" av

{-
mkAbiValueEncodingN :: Int -> Q [Dec]
mkAbiValueEncodingN nBy = do
  let tyNameS = T.pack $ "Bytes" ++ show nBy
  let tyNameT = return $ ConT $ mkName $ T.unpack tyNameS
  [d| instance AbiValueEncoding $tyNameT where
          toAbiValue = AVBytes . unBytesN
          fromAbiValue (AVBytes bs) = Right $ updateN (BytesN BS.empty) bs
          fromAbiValue av = fromAbiErr tyNameS av
      |]
-}

instance (KnownNat n) => IsString (BytesN n) where
  fromString = updateN emptyN . C8.pack

{-
mkIsStringN :: Int -> Q [Dec]
mkIsStringN nBy = do
  let tyNameS = "Bytes" ++ show nBy
  let tyNameT = return $ ConT $ mkName tyNameS
  [d| instance IsString $tyNameT where
          fromString = updateN (BytesN BS.empty) . C8.pack
      |]
-}

mkBytesNT :: Int -> Q Type
mkBytesNT nBy = return $ ConT $ mkName $ "Bytes" ++ show nBy

integer2bs = BS.pack . reverse . integer2w8
integer2w8 i
  | i == 0 = []
  | otherwise =
    let (c,r) = divMod i 256
    in fromIntegral r : integer2w8 c

bs2integer = w82integer . BS.unpack
w82integer = fst . foldr (\w8 (r,i) -> ((toInteger w8)*(256^i)+r,i+1)) (0,0)

fromUIntN :: (KnownNat m, KnownNat n) => UIntN m -> BytesN n
fromUIntN = updateN emptyN . integer2bs . toInteger

toUIntN :: (KnownNat m, KnownNat n) => BytesN m -> UIntN n
toUIntN = fromInteger . bs2integer . bytesN

instance (forall. KnownNat n) => Num (BytesN n) where
  (+) = iBinOp (+)
  (*) = iBinOp (*)
  abs = iMonOp abs
  negate = iMonOp (negateN $ natVal (Proxy :: (forall. KnownNat n) => Proxy n))
  signum = iMonOp signum
  fromInteger = updateN emptyN . integer2bs

negateN :: Integer -> Integer -> Integer
negateN nBy a
  | a == 0 = 0
  | a > 0 = 2^(nBy*8) - a
  | a < 0 = negate a

instance (forall. KnownNat n) => Real (BytesN n) where
  toRational (BytesN bs) = bs2integer bs % 1

instance (forall. KnownNat n) => Enum (BytesN n) where
  toEnum = updateN emptyN . integer2bs . toInteger
  fromEnum = fromIntegral . bs2integer . bytesN

instance (forall. KnownNat n) => Integral (BytesN n) where
  quotRem (BytesN bs1) (BytesN bs2) =
    let i1 = bs2integer bs1
        i2 = bs2integer bs2
    in ( updateN emptyN $ integer2bs $ i1 `quot` i2
       , updateN emptyN $ integer2bs $ i1 `rem` i2)
  toInteger = bs2integer . bytesN

instance (forall. KnownNat n) => Bits (BytesN n) where
  (.&.) = wBinOp (.&.)
  (.|.) = wBinOp (.|.)
  xor = wBinOp xor
  complement = wMonOp complement
  shift b i = iMonOp (flip shift i) b
  rotate b i = iMonOp (flip rotate i) b
  bitSize = (8*) . fromIntegral . natVal
  bitSizeMaybe = Just . bitSize
  isSigned _ = False
  testBit = testBitDefault
  bit = bitDefault
  popCount = popCountDefault

instance (forall. KnownNat n) => FiniteBits (BytesN n) where
  finiteBitSize = bitSize

wBinOp op b1 b2 = updateN emptyN
                $ BS.pack
                $ map (uncurry op)
                $ zip (BS.unpack $ bytesN b1) (BS.unpack $ bytesN b2)

wMonOp op = updateN emptyN . BS.pack . map op . BS.unpack . bytesN

iBinOp op b1 b2 = updateN emptyN $ integer2bs
                $ op (bs2integer $ bytesN b1) (bs2integer $ bytesN b2)

iMonOp op = updateN emptyN . integer2bs . op . bs2integer . bytesN

