{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

module Network.Web3.Dapp.Int
  ( IntN
  , UIntN
  , module Network.Web3.Dapp.Int.TH
  ) where

import Network.Web3.Dapp.Int.Internal
import Network.Web3.Dapp.Int.TH

