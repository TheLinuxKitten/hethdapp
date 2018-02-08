## Ethereum Dapp API

Interfaz de acceso a la utilidades **solc** y **swarm**.

Implementación de la especificación **ABI** de _Ethereum_:

* Codificación de llamadas a funciones

* Decodificación de resultados devueltos

* Decodificación de _logs_ emitidos por el _contract_

* Codificación de _filter topics_

Uso de _Template Haskell_ para generar una interfaz _Haskell_ de acceso a _contracts_ programados directamente con el lenguaje **Solidity**. La interfaz _Haskell_ incluye:

* el objeto _contract_

* función para su creación en el blockchain

* funciones para llamar a los métodos del _contract_ según las necesidades:
    
  * _call_ para funciones _pure_ o _view_

  * _sendTx_ para funciones _payable_ o _nonpayable_.

* tipo de datos y función para decodificar los logs emitidos por el _contract_

* tipo de datos y función para crear _filters_

* función para verificar el código binario del _contract_ almacenado en el blockchain

* función para subir a **swarm** el _metadata_ y los fuentes _Solidity_ usados en la compilación

* Posibilidad de generar interfaces para _contracts_ almacenados en el blockchain.

### Dependencias

* Compilador _solidity_ para compilar fuentes

* Nodo _Ethereum_ para la ejecución del monad _Web3T_

* Nodo _Swarm_ si se pretende subir el _metadata_ o descargarlo durante la compilación

### Ejemplo sencillo

El _contract_ `Coin` del proyecto **hsoldapps** está en el fichero fuente `coin.sol`:
```solidity
    pragma solidity ^0.4.0;

    contract Coin {
        // The keyword "public" makes those variables
        // readable from outside.
        address public minter;
        mapping (address => uint) public balances;

        // Events allow light clients to react on
        // changes efficiently.
        event Mint(address indexed to, uint amount);
        event Sent(address indexed from, address indexed to, uint indexed amount);

        // This is the constructor whose code is
        // run only when the contract is created.
        function Coin() public {
            minter = msg.sender;
        }

        function mint(address receiver, uint amount) public {
            require(msg.sender == minter);  //provoca fallo en eth_estimateGas
            //if (msg.sender != minter) return;
            balances[receiver] += amount;
            Mint(receiver, amount);
        }

        function send(address receiver, uint amount) public {
            if (balances[msg.sender] < amount) return;
            balances[msg.sender] -= amount;
            balances[receiver] += amount;
            Sent(msg.sender, receiver, amount);
        }
    }
```

El fuente se compila en el modulo `Coin.hs`:
```haskell
    {-# LANGUAGE FlexibleContexts #-}
    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE TemplateHaskell #-}

    module Ethereum.Solidity.Coin where

    import Language.Haskell.TH
    import Network.Web3.Dapp.EthABI.TH
    import Network.Web3.Dapp.EthABI.Types
    import System.Directory (getCurrentDirectory)

    $(runIO getCurrentDirectory >>= \wd -> compile (SolcSettings [] [])
        [ wd ++ "/src/Ethereum/Solidity/coin.sol"
        ])
```

La compilación (ver documentación de la función `compile`) produce la siguiente interfaz _Haskell_:
```haskell
data Coin_Event
      = Coin_Mint (HexEthAddr, Integer) |
        Coin_Sent (HexEthAddr, HexEthAddr, Integer)
      deriving (Show)
data Coin_Event_Filter
      = Coin_Mint_Filter (Maybe HexEthAddr) |
        Coin_Sent_Filter (Maybe HexEthAddr, Maybe HexEthAddr, Maybe Integer)
      deriving (Show)
type Coin_minter_Out = HexEthAddr
type Coin_balances_In = HexEthAddr
type Coin_balances_Out = Integer
type Coin_mint_In = (HexEthAddr, Integer)
type Coin_send_In = (HexEthAddr, Integer)
coin_contract :: Contract
coin_new_in :: HexData
coin_new_sendtx :: HexEthAddr -> (HexEthAddr, Maybe HexEthAddr, Maybe Integer, Maybe HexData)
coin_mint_in :: Coin_mint_In -> HexData
coin_mint_sendtx :: HexEthAddr -> HexEthAddr -> Coin_mint_In -> (HexEthAddr, Maybe HexEthAddr, Maybe Integer, Maybe HexData)
coin_send_sendtx :: HexEthAddr -> HexEthAddr -> Coin_send_In -> (HexEthAddr, Maybe HexEthAddr, Maybe Integer, Maybe HexData)
coin_send_in :: Coin_send_In -> HexData
coin_balances_out :: HexData -> Coin_balances_Out
coin_balances_in :: Coin_balances_In -> HexData
coin_balances_call :: (MonadBaseControl IO m, MonadLoggerIO m, JsonRpcConn c) => HexEthAddr -> HexEthAddr -> Coin_balances_In -> ReaderT * (Web3Session c m) (JsonRpcConnT c m) Coin_balances_Out
coin_minter_out :: HexData -> Coin_minter_Out
coin_minter_in :: HexData
coin_minter_call :: (MonadBaseControl IO m, MonadLoggerIO m, JsonRpcConn c) => HexEthAddr -> HexEthAddr -> ReaderT * (Web3Session c m) (JsonRpcConnT c m) Coin_minter_Out
coin_to_filter_topics :: Coin_Event_Filter -> [RpcEthFilterTopic]
coin_decode_log :: RpcEthLog -> Either Text Coin_Event
coin_from_log :: Text -> AbiValue -> Either Text Coin_Event
coin_guard :: (MonadBaseControl IO m, MonadLoggerIO m, JsonRpcConn c) => HexEthAddr -> Web3T c m ()
coin_swarm_upload :: IO (Either Text (HexHash256, [(FilePath, HexHash256)]))
```

