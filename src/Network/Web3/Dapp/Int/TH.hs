{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

module Network.Web3.Dapp.Int.TH where

import Network.Web3.Dapp.Int.Internal

$(concat <$> mapM mkIntN (map (*8) [1..32]))
$(concat <$> mapM mkUIntN (map (*8) [1..32]))

