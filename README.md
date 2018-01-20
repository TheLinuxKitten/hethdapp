## Ethereum Dapp API

Interfaz de acceso a la utilidades **solc** y **swarm**.

Implementación de la especificación **ABI** de _Ethereum_:

    * Codificación de llamadas a funciones

    * Decodificación de resultados devueltos

    * Decodificación de _logs_ emitidos por el _contract_

    * Codificación de _filter topics_

Uso de _Template Haskell_ para generar una interfaz _Haskell_ de acceso a
_contracts_ programados directamente con el lenguaje **Solidity**. La
interfaz _Haskell_ incluye:

    * el objeto _contract_

    * función para su creación en el blockchain

    * funciones para llamar a los métodos del _contract_ según las necesidades:
    
        * _call_ para funciones _pure_ o _view_

        * _sendTx_ para funciones _payable_ o _nonpayable_.

    * función para decodificar los logs emitidos por el _contract_

    * función para crear _filters_

    * función para verificar el código binario del _contract_ almacenado
        en el blockchain

    * función para subir a **swarm** el _metadata_ y los fuentes _Solidity_
        usados en la compilación

Posibilidad de generar interfaces para _contracts_ almacenados
en el blockchain.

### Dependencias

Compilador _solidity_ para compilar fuentes

Nodo _Ethereum_ para la ejecución del monad _Web3T_

Nodo _Swarm_ si se pretende subir el _metadata_ o descargarlo durante
la compilación

