{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
--{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_HADDOCK hide #-}

--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

module Network.Web3.Dapp.FixArray.Internal
  ( FixArray(..)
  , fromFixArray
  , toFixArray
  , toFixArray'
  , mkFixArray
  ) where

import Data.Proxy
import Data.Monoid ((<>))
import qualified Data.Sized.Builtin as SB
import qualified Data.Text as T
--import GHC.Exts
import GHC.TypeLits
import Language.Haskell.TH.Syntax
import Network.Web3.Dapp.EthABI.Types hiding (Type)

type FixArray (n :: Nat) a = SB.Sized [] n a

toFixArray :: forall n a. (KnownNat n) => [a] -> Either T.Text (FixArray n a)
toFixArray xs =
  let lenN = fromIntegral $ natVal (Proxy :: Proxy n)
      lenXs = length xs
  in if lenXs /= lenN
      then Left $ "toFixArray: Unexpected list length "
               <> T.pack (show lenXs)
               <> ", expected "
               <> T.pack (show lenN)
      else Right (SB.unsafeFromList' xs)

toFixArray' :: forall n a. (KnownNat n) => [a] -> FixArray n a
toFixArray' = either (error . T.unpack) id . toFixArray

fromFixArray :: (KnownNat n) => FixArray n a -> [a]
fromFixArray = SB.toList

instance (KnownNat n, AbiValueEncoding a) => AbiValueEncoding (FixArray n a) where
  toAbiValue = toAbiValue . fromFixArray
  fromAbiValue av@(AVArray ar) = fromAbiValue av >>= toFixArray
  fromAbiValue av = fromAbiErr "FixArray n a" av

mkFixArray :: Int -> Q Type -> Q Type
mkFixArray n aT = do
  let nT = return $ LitT $ NumTyLit $ toInteger n
  [t| FixArray $(nT) $(aT) |]

{-
instance forall n a. (KnownNat n) => IsList (FixArray n a) where
  type Item (FixArray n a) = a
  fromList :: [a] -> FixArray n a
  fromList = either (error . T.unpack) id . toFixArray
  toList :: FixArray n a -> [a]
  toList = fromFixArray
-}

