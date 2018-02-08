{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

module Network.Web3.Dapp.FixArray
  ( FixArray(..)
  , pattern NilL
  , pattern NilR
  , (S.|>)
  , (S.<|)
  , fromFixArray
  , toFixArray
  , toFixArray'
  ) where

import qualified Data.Sized as S
import Network.Web3.Dapp.FixArray.Internal

pattern NilL :: forall a. FixArray 0 a
pattern NilL = S.NilL

pattern NilR :: forall a. FixArray 0 a
pattern NilR = S.NilR

