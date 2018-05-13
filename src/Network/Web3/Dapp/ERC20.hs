{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

-- | Este módulo implementa funciones para consultar la especificación EIP20
-- o ERC20
module Network.Web3.Dapp.ERC20
  ( erc20Info
  , erc20Selector
  , erc20Interface
  , erc20IsRequired
  , erc20IsFunction
  , erc20IsEvent
  ) where

import qualified Data.ByteString as BS
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromJust)
import Data.Text (Text)
import Network.Web3.Dapp.ERC20.Interface
import Network.Web3.Dapp.EthABI
import Network.Web3.Dapp.EthABI.Types

type Erc20Info = (BS.ByteString, Interface, Bool, Bool)

erc20Info :: HM.HashMap Text Erc20Info
erc20Info = HM.fromList erc20Abi

-- | Devuelve el selecctor del `Interface` (función o evento)
erc20Selector :: Erc20Info -> BS.ByteString
erc20Selector (fs,_,_,_) = fs

-- | Devuelve el `Interface`
erc20Interface :: Erc20Info -> Interface
erc20Interface (_,i,_,_) = i

-- | Indica si es un `Interface` de presencia obligatoria
erc20IsRequired :: Erc20Info -> Bool
erc20IsRequired (_,_,r,_) = r

-- | Indica si el `Interface` es una función. Si no es un evento.
erc20IsFunction :: Erc20Info -> Bool
erc20IsFunction (_,_,_,r) = r

-- | Indica si el `Interface` es un evento.
erc20IsEvent :: Erc20Info -> Bool
erc20IsEvent = not . erc20IsFunction

erc20Abi =
  [ (nameFun, (functionSelector $ getFun nameFun, IFunction $ getFun nameFun, False, True))
  , (symbolFun, (functionSelector $ getFun symbolFun, IFunction $ getFun symbolFun, False, True))
  , (decimalsFun, (functionSelector $ getFun decimalsFun, IFunction $ getFun decimalsFun, False, True))
  , (totalSupplyFun, (functionSelector $ getFun totalSupplyFun, IFunction $ getFun totalSupplyFun, True, True))
  , (balanceOfFun, (functionSelector $ getFun balanceOfFun, IFunction $ getFun balanceOfFun, True, True))
  , (transferFun, (functionSelector $ getFun transferFun, IFunction $ getFun transferFun, True, True))
  , (transferFromFun, (functionSelector $ getFun transferFromFun, IFunction $ getFun transferFromFun, True, True))
  , (approveFun, (functionSelector $ getFun approveFun, IFunction $ getFun approveFun, True, True))
  , (allowanceFun, (functionSelector $ getFun allowanceFun, IFunction $ getFun allowanceFun, True, True))
  , (transferEvt, (functionSelector $ getEvt transferEvt, IEvent $ getEvt transferEvt, True, False))
  , (approvalEvt, (functionSelector $ getEvt approvalEvt, IEvent $ getEvt approvalEvt, True, False))
  ]

getFun nom = fromJust $ lookupFunction nom eip20interface_contract
getEvt nom = fromJust $ lookupEvent nom eip20interface_contract

nameFun = "name"
symbolFun = "symbol"
decimalsFun = "decimals"
totalSupplyFun = "totalSupply"
balanceOfFun = "balanceOf"
transferFun = "transfer"
transferFromFun = "transferFrom"
approveFun = "approve"
allowanceFun = "allowance"
transferEvt = "Transfer"
approvalEvt = "Approval"

