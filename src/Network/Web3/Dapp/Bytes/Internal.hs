{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
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
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Monoid ((<>))
import qualified Data.Text as T
import GHC.Exts
import GHC.TypeLits (KnownNat, Nat, natVal)
import Language.Haskell.TH.Syntax
import Network.Web3.Dapp.EthABI.Types hiding (Type)

newtype BytesN (n :: Nat) = BytesN { unBytesN :: BS.ByteString }
                          deriving (Eq, Ord, Read, Show)

bytesN :: (KnownNat n) => BytesN n -> BS.ByteString
bytesN = unBytesN

lengthN :: (KnownNat n) => BytesN n -> Int
lengthN = fromIntegral . natVal

zeroN :: (KnownNat n) => BytesN n
zeroN = updateN (BytesN BS.empty) BS.empty

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
  fromAbiValue (AVBytes bs) = Right $ updateN zeroN bs
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
  fromString = updateN zeroN . C8.pack

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

