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

module Network.Web3.Dapp.EthABI.TH
  ( compile
  , downloadHttp
  , downloadIpc
  , fromJust
  , fromRight
  , joinHex
  , abiCallDataToHexText
  , encodeAbi
  , decodeAbi
  , web3_call
  , web3_callPure
  , defaultSwarmSettings
  , HexEthAddr(..)
  , Solc.SolcSettings(..)
  , Solc.SolcRemapping(..)
  , SwarmSettings(..)
  , FixArray(..)
  , module Network.Web3.Dapp.Bytes.TH
  , module Network.Web3.Dapp.Int.TH
  ) where

import Control.Arrow ((&&&))
import Control.Monad (unless)
import Control.Monad.Logger
import qualified Data.ByteString as BS
import Data.Char (toLower, toUpper)
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.List (partition)
import Data.Maybe (fromJust, isJust)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Word
import Language.Haskell.TH
import Network.Web3.Extra
  ( web3_call
  , web3_callPure
  , web3_estimateAndSendTx'
  , web3_estimateAndSendTxs
  , web3_guardBin
  )
import Network.Web3
import Network.Web3.Dapp.Bytes.TH
import Network.Web3.Dapp.Bytes.Internal
import Network.Web3.Dapp.EthABI
import Network.Web3.Dapp.EthABI.Bzz
import Network.Web3.Dapp.EthABI.Types hiding (Type)
import qualified Network.Web3.Dapp.EthABI.Types as Abi
import Network.Web3.Dapp.FixArray.Internal
import Network.Web3.Dapp.Int.TH
import Network.Web3.Dapp.Int.Internal
import qualified Network.Web3.Dapp.Solc as Solc
import Network.Web3.Dapp.Swarm (SwarmSettings(..),defaultSwarmSettings)
import Network.Web3.HexText
import Network.Web3.Types

-- | Compila una lista de fuentes Solidity y devuelve una lista de declaraciones
-- de símbolos para interactuar con los contracts.
--
-- Por cada contract Solidity, genera:
--
--    * El objeto 'Contract'
--
--    * Genera una función ___guard__ para verificar que el código del
--      contract en el /blockchain/ coincide con el contract compilado.
--      La función espera la dirección del contract en el blockchain.
--
--    * Para cada función genera los tipos y funciones necesarios para
--      codificar los datos de llamada y decodificar los datos producidos
--      como resultado. Los tipos tienen el sufijo ___In__/___Out__ y las
--      funciones el sufijo ___in__/___out__.
--
--    * Para las funciones de tipo /view/ genera una función ___call__ que
--      recibe los parámetros de entrada necesarios (la dirección del emisor,
--      la dirección del contract y los parámetros de entrada si los tiene)
--      y devuelve la salida. Ver `web3_call`.
--
--    * Para las funciones de tipo /pure/ también genera una función
--      ___call_pure__. Esta función no espera la dirección del emisor.
--      Ver `web3_callPure`.
--
--    * Para las funciones de tipo /nonpayable/ y /payable/ genera una
--      función ___call__, y una función ___sendtx__ que devuelve los datos
--      necesarios (una tupla de cuatro valores) para hacer una transacción
--      (Ver 'web3_estimateAndSendTx'' y 'web3_estimateAndSendTxs').
--
--    * Las funciones ___call__ y ___call_pure__ se generan si la función
--      devuelve parámetros, o sea, tiene /outputs/.
--
--    * El constructor se trata como una función cuyo nombre es __new__.
--      La función espera la dirección del emisor y los parámetros de entrada,
--      si los tiene.
--
--    * Genera un nuevo tipo para representar los eventos de los logs, y lo
--      instancia a la clase 'FromLogEvent' para decodificar los logs. Ver
--      'decodeLogs' y 'DecodeLogResult'. Cada evento del contract se representa
--      mediante un constructor en el que sus argumentos son los parámetros
--      del evento.
--
--    * Genera función ___from_log__ que realiza una llamada prototipada
--      a `fromLogEvent`.
--
--    * Genera función ___decode_log__ que decodifica directamente `RpcEthLog`.
--
--    * Genera un nuevo tipo para representar los filter topics de un evento,
--      y lo instancia a la clase 'ToEventFilter' para codificar los topics.
--      Ver 'encodeEventFilter'. Cada evento del contract se representa
--      mediante un constructor en el que sus argumentos son los parámetros
--      indexed del evento, con el añadido de ser `Maybe` para poder definir su
--      ausencia (null topic).
--
--    * Genera función ___to_filter_topics__ que usa la instancia a
--      `ToEventFilter` para codificar los topics.
--
--    * Genera una función ___swarm_upload__ que sube a /swarm/ el metadata
--      y los fuentes compilados.
--
compile :: Solc.SolcSettings -> [FilePath] -> Q [Dec]
compile stgs fps = do
  (econtracts,_,_) <- runIO $ Solc.compile stgs fps
  case econtracts of
    Left err -> fail (T.unpack err)
    Right contracts -> concat <$> mapM genContract contracts

download :: NoLoggingT IO (Either Text HexData) -> SwarmSettings -> Q [Dec]
download f swarmOps = do
  eDownMeta <- runIO $ do
    eBinCode <- runNoLoggingT f
    case eBinCode of
      Left e1 -> return $ Left e1
      Right binCode -> downloadMetadata swarmOps binCode
  case eDownMeta of
    Left e2 -> fail (T.unpack e2)
    Right (contract,_) -> genContract contract

ethGetCode :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
           => HexEthAddr -> Web3T c m HexData
ethGetCode addr = eth_getCode addr RPBLatest

-- | Devuelve una lista de declaraciones de símbolos para interactuar con
-- un contract (Ver `compile`).
--
--  Dada la dirección de un contract /Ethereum/, la función:
--
--    1. __descarga__, del blockchain, el código binario del contract
--
--    2. obtiene la url /bzzr0/ contenida en el código
--
--    3. descarga el `Metadata` de /swarm/, y finalmente
--
--    4. devuelve la declaración del `Contract` y su interfaz.
-- 
-- No genera la función de creación del contract en el blockchain. Pero se
-- puede usar el contenido de los fuentes (ver `downloadMetadata`) para
-- volver a compilar el contract.
--
-- Esta función usa la interfaz HTTP del nodo, y como es lógico, el nodo
-- debe estar iniciado y escuchando sino la compilación fallará.
--
--
downloadHttp :: String -> SwarmSettings -> HexEthAddr -> Q [Dec]
downloadHttp url ops addr = download (runWeb3HttpT 5 5 url $ ethGetCode addr) ops

-- | Interfaz IPC. Ver `downloadHttp`.
downloadIpc :: FilePath -> SwarmSettings -> HexEthAddr -> Q [Dec]
downloadIpc fp ops addr = download (runWeb3IpcT 5 5 fp $ ethGetCode addr) ops

ethName :: [Text] -> String
ethName = T.unpack . T.intercalate "_"

ethNameT :: [Text] -> Name
ethNameT = mkName . (\s -> (toUpper $ head s) : tail s) . ethName

ethNameD :: [Text] -> Name
ethNameD = mkName . ethName . map (T.map toLower)

genContract :: Contract -> Q [Dec]
genContract contract = do
  let abi = abiContractAbi contract
  let conName = abiContractName contract
  let conNameD = ethNameD [conName,"Contract"]
  conT <- [t| Contract |]
  conE <- [| contract |]
  let conD =
        [ SigD conNameD conT
        , FunD conNameD [Clause [] (NormalB conE) []]
        ]
  conGuardD <- genGuard conName conNameD
  conEventsD <- genEvents conName
                              ( map abiInterfaceEvent
                              $ filter isInterfaceEvent abi)
  let abiCons = map abiInterfaceConstructor $ filter isInterfaceConstructor abi
  let cons = if null abiCons
              then [Constructor [] False SMNonPayable False]
              else abiCons
  conConstrsD <- if isJust (abiContractBin contract)
                  then concat <$> mapM (genConstr conName) cons
                  else return []
  let funcs = map abiInterfaceFunction $ filter isInterfaceFunction abi
  let (_,overNoms) = foldr getOverFunc ([],[]) funcs
  let (funcs1,funcs2) = partition (not . isOverFunc overNoms) funcs
  let func1Noms = map abiFuncName funcs1
  let (func2Noms,_) = foldr addOverFunc ([],initOverFuncNoms overNoms) funcs2
  conFuncs1D <- concat <$> mapM (genFunc conName) (zip func1Noms funcs1)
  conFuncs2D <- concat <$> mapM (genFunc conName) (zip func2Noms funcs2)
  return $ conD ++ conGuardD ++ conEventsD ++ conConstrsD ++ conFuncs1D ++ conFuncs2D
  where
    getOverFunc f (ns,ons) =
      let fNom = abiFuncName f
      in if elem fNom ns then (ns,fNom:ons) else (fNom:ns,ons)
    isOverFunc overNoms f = abiFuncName f `elem` overNoms
    initOverFuncNoms overNoms = HM.fromList $ zip overNoms $ repeat 1
    addOverFunc f (overFuncNoms,hmNoms) =
      let fNom = abiFuncName f
          idx = hmNoms HM.! fNom
      in ( fNom <> T.pack (show idx) : overFuncNoms
         , HM.update (const $ Just $ idx+1) fNom hmNoms
         )

genGuard :: Text -> Name -> Q [Dec]
genGuard conName conNameD = do
  let guardNom = ethNameD [conName,"Guard"]
{-
  let guardNomD = return $ VarP $ guardNom
  let conE = return $ VarE conNameD
  [d| $guardNomD = web3_guardBin (abiContractBinRuntime $conE) |]
-}
  let addrNom = mkName "addr"
  let addrP = VarP addrNom
  let addrE = return $ VarE addrNom
  let conE = return $ VarE conNameD
  bodyE <- [| web3_guardBin (abiContractBinRuntime $conE) $addrE |]
  return $ [FunD guardNom [Clause [addrP] (NormalB bodyE) []]]

appDatE lenIps funDatInD datD =
  if lenIps > 0 then AppE funDatInD (VarE datD) else funDatInD

genCall :: Bool -> Text -> Text -> Name -> Name -> Name -> Exp -> [Param] -> Q [Dec]
genCall isPure conName funName fromA toA datD funDatInD ips = do
  let lenIps = length ips
  let appDatD = appDatE lenIps funDatInD datD
  let funDatOutD = VarE $ ethNameD [conName,funName,"Out"]
  opE <- [| (<$>) |]
  ethCallE1 <- [| web3_call |]
  let opCallE1 = AppE (AppE (AppE ethCallE1 (VarE fromA)) (VarE toA)) appDatD
  let bodyExp1 = UInfixE funDatOutD opE opCallE1
  let funNameD1 = ethNameD [conName,funName,"Call"]
  let call1 = FunD funNameD1 [Clause ([VarP fromA, VarP toA]++[VarP datD|lenIps>0]) (NormalB bodyExp1) []]
  call2 <- if isPure
            then do
              ethCallE2 <- [| web3_callPure |]
              let opCallE2 = AppE (AppE ethCallE2 (VarE toA)) appDatD
              let bodyExp2 = UInfixE funDatOutD opE opCallE2
              let funNameD2 = ethNameD [conName,funName,"Call","Pure"]
              return [FunD funNameD2 [Clause ([VarP toA]++[VarP datD|lenIps>0]) (NormalB bodyExp2) []]]
            else return []
  return (call1:call2)

genSendTx :: Bool -> Bool -> Text -> Text -> Name -> Name -> Name -> Exp -> [Param] -> Q [Dec]
genSendTx isPayable isCons conName funName fromA toA datD funDatInD ips = do
  let funNameD = ethNameD [conName,funName,"SendTx"]
  let valueD = mkName "value"
  let lenIps = length ips
  hexAddrT <- [t| HexEthAddr |]
  integerT <- [t| Integer |]
  let conTyNameT = ConT $ ethNameT [conName,funName,"In"]
  mayHexAddrT <- [t| Maybe HexEthAddr |]
  mayIntegerT <- [t| Maybe Integer |]
  mayHexDataT <- [t| Maybe HexData |]
  let funSigDat = [AppT ArrowT hexAddrT]
               ++ [AppT ArrowT hexAddrT|not isCons]
               ++ [AppT ArrowT integerT|isPayable]
               ++ [AppT ArrowT conTyNameT|lenIps > 0]
  let funSigResp = foldl AppT (TupleT 4)
                      [hexAddrT, mayHexAddrT, mayIntegerT, mayHexDataT]
  let funSigD = SigD funNameD $ foldr AppT funSigResp funSigDat
  let funArgsP = [VarP fromA] ++ [VarP toA|not isCons]
              ++ [VarP valueD|isPayable] ++ [VarP datD|lenIps > 0]
  justE' <- [| Just |]
  nothingE <- [| Nothing |]
  let justE e = AppE justE' e
  let appDatD = appDatE lenIps funDatInD datD
  let tupE = [VarE fromA]
          ++ [if isCons then nothingE else justE $ VarE toA]
          ++ [if isPayable then (justE $ VarE valueD) else nothingE]
          ++ [justE appDatD]
  return [funSigD, FunD funNameD [Clause funArgsP (NormalB $ TupE tupE) []]]

genFuncCall :: StateMutability -> Bool -> Text -> Text
            -> [Param] -> [Param] -> Q [Dec]
genFuncCall stm isCons conName funName ips ops = do
  let fromA = mkName "fromAddr"
  let toA = mkName "toAddr"
  let datD = mkName "dat"
  let funDatInD = VarE $ ethNameD [conName,funName,"In"]
  let lenIps = length ips
  case stm of
    SMPure -> case length ops of
      0 -> return []
      _ -> genCall True conName funName fromA toA datD funDatInD ips
    SMView -> case length ops of
      0 -> return []
      _ -> genCall False conName funName fromA toA datD funDatInD ips
    SMNonPayable -> do
      txD <- genSendTx False isCons conName funName fromA toA datD funDatInD ips
      callD <- genFuncCall SMView isCons conName funName ips ops
      return $ txD ++ callD
    SMPayable -> do
      txD <- genSendTx True isCons conName funName fromA toA datD funDatInD ips
      callD <- genFuncCall SMView isCons conName funName ips ops
      return $ txD ++ callD

genConstr :: Text -> Constructor -> Q [Dec]
genConstr conName constructor = do
  let ips = idxParams False $ abiConsInputs constructor
  conTyD <- genSynParams conName "New" "In" ips
  let conE = return $ VarE $ ethNameD [conName,"Contract"]
  let conTyNameT = ConT $ ethNameT [conName,"New","In"]
  let conNameD = ethNameD [conName,"New","In"]
  let consE = [| IConstructor constructor |]
  resT <- [t| HexData |]
  let lenIps = length ips
  let conFunT = case lenIps of
                  0 -> resT
                  _ -> AppT (AppT ArrowT conTyNameT) resT
  let funInPre = [| ((fromJust $ abiContractBin $conE)<>) . abiCallDataToHexText . fromRight . encodeAbi $consE |]
  let funInBody = case lenIps of
                    0 -> [| $funInPre arg0 |]
                    1 -> [| $funInPre . arg1 |]
                    _ -> [| $funInPre |]
  conFunD <- (SigD conNameD conFunT :)
         <$> [d| $(return $ VarP conNameD) = $funInBody |]
  conFunCallD <- genFuncCall (abiConsStateMutability constructor) True conName "New" (abiConsInputs constructor) []
  let upNameD = return $ VarP $ ethNameD [conName,"Swarm","Upload"]
  upFunD <- [d| $upNameD = uploadMetadata defaultSwarmSettings (fromJust $ abiContractBin $conE) (abiContractMetadata $conE) |]
  return $ conTyD ++ conFunD ++ conFunCallD ++ upFunD

genFunc :: Text -> (Text,Function) -> Q [Dec]
genFunc conName (funcName,function) = do
  let ips = idxParams False $ abiFuncInputs function
  let ops = idxParams False $ abiFuncOutputs function
  let funcNameD = [| funcName |]
  inTyD <- genSynParams conName funcName "In" ips
  outTyD <- genSynParams conName funcName "Out" ops
  resT <- [t| HexData |]
  let funInTyNameT = ConT $ ethNameT [conName,funcName,"In"]
  let funInNameD = ethNameD [conName,funcName,"In"]
  let lenIps = length ips
  let lenOps = length ops
  let funInFunT = case lenIps of
                    0 -> resT
                    _ -> AppT (AppT ArrowT funInTyNameT) resT
  let funE = [| IFunction function |]
  let funInPre = [| joinHex . abiCallDataToHexText . fromRight . encodeAbi $funE |]
  let funInBody = case lenIps of
                    0 -> [| $funInPre arg0 |]
                    1 -> [| $funInPre . arg1 |]
                    _ -> [| $funInPre |]
  funInFunD <- (SigD funInNameD funInFunT :)
           <$> [d| $(return $ VarP funInNameD) = $funInBody |]
  let funOutTyNameT = ConT $ ethNameT [conName,funcName,"Out"]
  let funOutNameD = ethNameD [conName,funcName,"Out"]
  funOutFunD <- case length ops of
                  0 -> return []
                  otherwise -> (SigD funOutNameD (AppT (AppT ArrowT resT) funOutTyNameT) :) <$> [d| $(return $ VarP funOutNameD) = fromRight . decodeAbi $funE |]
  conFunCallD <- genFuncCall (abiFuncStateMutability function) False conName funcName (abiFuncInputs function) (abiFuncOutputs function)
  return $ inTyD ++ outTyD ++ funInFunD ++ funOutFunD ++ conFunCallD

genEvents :: Text -> [Event] -> Q [Dec]
genEvents conName events = if length events == 0
  then return []
  else do
    (evdNameD, eventD) <- genEventType False conName events
    let evdNameT = return $ ConT evdNameD
    evInsT <- [t| FromLogEvent $evdNameT |]
    evInsDecD <- mapM (genEventInstance conName) events
    let evInsD = InstanceD Nothing [] evInsT
                    [FunD (mkName "fromLogEvent") evInsDecD]
    let evInsFunName = ethNameD [conName,"From","Log"]
    evInsFunSigT <- [t| Text -> AbiValue -> Either Text $evdNameT |]
    let evInsFunSigD = SigD evInsFunName evInsFunSigT
    let evInsFunD = ValD (VarP evInsFunName)
                         (NormalB $ VarE $ mkName "fromLogEvent") []
    let conE = return $ VarE $ ethNameD [conName,"Contract"]
    let evInsFunName2 = ethNameD [conName,"Decode","Log"]
    let evInsFunNameD2 = return $ VarP evInsFunName2
    evInsFunT2 <- [t| RpcEthLog -> Either Text $evdNameT |]
    let evInsFunSigD2 = SigD evInsFunName2 evInsFunT2
    evInsFunD2 <- [d| $evInsFunNameD2 = (\edlr -> edlr >>= uncurry $(return $ VarE $ mkName "fromLogEvent")) . fromJust . decodeLog (abiContractAbi $conE) |]
    (evdNameMD, eventMD) <- genEventType True conName events
    let evInsMNameT = return $ ConT evdNameMD
    evInsMT <- [t| ToEventFilter $evInsMNameT |]
    evInsMDecD <- mapM (genEventInsM conName) events
    let evInsMD = InstanceD Nothing [] evInsMT
                    [FunD (mkName "toEventFilter") evInsMDecD]
    let evInsMFunName = ethNameD [conName,"To","Filter","Topics"]
    evInsMFunSigRet <- [t| $evInsMNameT -> [RpcEthFilterTopic] |]
    let evInsMFunSigD = SigD evInsMFunName evInsMFunSigRet
    let evInsMFunNameD = return $ VarP evInsMFunName
    evInsMFunD <- [d| $evInsMFunNameD = fromRight . fromJust . uncurry (encodeEventFilter (abiContractAbi $conE)) . toEventFilter |]
    return $ [eventD, evInsD, evInsFunSigD, evInsFunD, evInsFunSigD2] ++ evInsFunD2 ++ [eventMD, evInsMD, evInsMFunSigD] ++ evInsMFunD
  where
    genEventType isM conName' event' = do
      let evdNameD = ethNameT ([conName,"Event"]++["Filter"|isM])
      eventValuesC <- mapM (genEventConstructor isM conName) events
      showT <- [t| Show |]
      let eventD = DataD [] evdNameD [] Nothing eventValuesC [showT]
      return (evdNameD, eventD)
    genEventConstructor :: Bool -> Text -> Event -> Q Con
    genEventConstructor isM conName ev = do
      let evdcNameD = ethNameT ([conName,abiEventName ev]++["Filter"|isM])
      evdcPsT <- genEventParams isM (abiEventInputs ev)
      return $ NormalC evdcNameD (map bangT evdcPsT)
    bangT evdcpT = (Bang NoSourceUnpackedness NoSourceStrictness, evdcpT)
    genEventInstance :: Text -> Event -> Q Clause
    genEventInstance conName ev = do
      let evName = abiEventName ev
      let valP = LitP $ StringL (T.unpack evName)
      let func2D = ConE $ ethNameT [conName,evName]
      op <- [| (<$>) |]
      fav <- [| fromAbiValue |]
      let b = UInfixE func2D op (AppE fav (VarE (mkName "av")))
      return $ Clause [valP, VarP (mkName "av")] (NormalB b) []
    genEventInsM :: Text -> Event -> Q Clause
    genEventInsM conName ev = do
      let evName = abiEventName ev
      let evNameP = LitP $ StringL (T.unpack evName)
      let evNameE = LitE $ StringL (T.unpack evName)
      let pName = ethNameT [conName,evName,"Filter"]
      let ips = eventIndexedParams $ abiEventInputs ev
      let (argP,ipsE) = case length ips of
                          0 -> ([], ListE [])
                          lenIps -> ( [VarP $ mkName "a"]
                                    , AppE (VarE $ mkName $ "toMaybeAbiValue" ++ show lenIps) (VarE $ mkName "a") )
      return $ Clause [ConP pName argP] (NormalB $ TupE [evNameE, ipsE]) []
    genEventParams :: Bool -> [EventParam] -> Q [Type]
    genEventParams isM evps = do
      let ps = filter (\(isI,_) -> not isM || isI)
             $ map (abiEventParamIndexed &&& abiEventParam) evps
      if null ps then return [] else (:[]) <$> genParams isM ps

idxParams :: Bool -> [Param] -> [(Bool,Param)]
idxParams idx = zip (repeat idx)

genSynParams :: Text -> Text -> Text -> [(Bool,Param)] -> Q [Dec]
genSynParams conName funcName sufijo ips = case length ips of
  0 -> return []
  otherwise -> do
    let psNameT = ethNameT [conName,funcName,sufijo]
    psT <- genParams False ips
    return [TySynD psNameT [] psT]

genParams :: Bool -> [(Bool,Param)] -> Q Type
genParams isM ips = case length ips of
  0 -> fail "genParams': Empty params!!!"
  1 -> genParam isM $ head ips
  lenPs -> foldl AppT (TupleT lenPs) <$> mapM (genParam isM) ips

genParam :: Bool -> (Bool,Param) -> Q Type
genParam isM (idx,p) =
  if not isM && eventParamValueIsHash256 idx p
    then [t| HexHash256 |]  {- genType isM idx TyUtf8 NoArray -}
    else genType isM idx (abiParamType p) (abiParamTypeArray p)

genType :: Bool -> Bool -> Abi.Type -> TypeArray -> Q Type
genType isM idx ty tya = case tya of
  DynamicArray -> genList idx ty >>= genMT isM
  FixedArray n -> genFixArray n idx ty >>= genMT isM
  NoArray -> case ty of
    (TyTuple cs) -> genParams isM $ idxParams idx cs
    (TyInt sg sz) -> genInt sg sz >>= genMT isM
    TyAddress -> genAddr >>= genMT isM
    TyBool -> genBool >>= genMT isM
    (TyFixed sg m n) -> genFrac >>= genMT isM
    (TyBin sz) -> genBin sz >>= genMT isM
    TyBytes -> genBytes >>= genMT isM
    TyUtf8 -> genString >>= genMT isM
    TyFunction -> fail "genType: Function not implemmented!!!"
  where
    genMT isM t = if isM then flip AppT t <$> [t| Maybe |] else return t
    genList idx' ty' = AppT ListT <$> genType False idx' ty' NoArray
    genFixArray n idx' ty' = mkFixArray n $ genType False idx' ty' NoArray
    genAddr = [t| HexEthAddr |]
    genBool = [t| Bool |]
    genFrac = [t| Double |]
    genBin = mkBytesNT
    genBytes = [t| BS.ByteString |]
    genString = [t| T.Text |]
    genInt sg = if sg then mkIntNT else mkUIntNT
    {-
    genInt sg sz =
      if sg
        then genIntSz sz [t| Int8 |]  [t| Int16 |]  [t| Int32 |]  [t| Int64 |]
        else genIntSz sz [t| Word8 |] [t| Word16 |] [t| Word32 |] [t| Word64 |]
    genIntSz sz t8 t16 t32 t64 =
      if sz <= 8 then t8
        else if sz <= 16 then t16
              else if sz <= 32 then t32
                    else if sz <= 64 then t64
                          else [t| Integer |]
    -}

