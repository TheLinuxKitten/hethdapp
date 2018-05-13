{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

-- | Módulo que implementa el interfaz ERC20/EIP20 de los contracts Ethereum
module Network.Web3.Dapp.ERC20.Interface where

import Language.Haskell.TH
import Network.Web3.Dapp.EthABI.TH
import Network.Web3.Dapp.EthABI.Types
import System.Directory (getCurrentDirectory)

$(runIO getCurrentDirectory >>= \wd -> compileInterface (SolcSettings [] [])
    [ wd ++ "/src/Network/Web3/Dapp/ERC20/EIP20Interface.sol"
    ])

