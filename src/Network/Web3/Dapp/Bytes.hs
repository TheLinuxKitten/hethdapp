{-# LANGUAGE DataKinds #-}

--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

module Network.Web3.Dapp.Bytes
  ( BytesN
  , bytesN
  , zeroN
  , updateN
  , lengthN
  , fromUIntN
  , toUIntN
  , module Network.Web3.Dapp.Bytes.TH
  ) where

import Network.Web3.Dapp.Bytes.Internal
import Network.Web3.Dapp.Bytes.TH

