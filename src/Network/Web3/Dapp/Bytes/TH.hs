{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

module Network.Web3.Dapp.Bytes.TH where

import Network.Web3.Dapp.Bytes.Internal

$(concat <$> mapM mkBytesN [1..32])
{-
$(concat <$> mapM mkAbiValueEncodingN [1..32])
$(concat <$> mapM mkIsStringN [1..32])
-}

