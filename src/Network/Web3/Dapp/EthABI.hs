{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

module Network.Web3.Dapp.EthABI
    ( decodeSolcAbi
    , decodeSolcAst
    , decodeMetadata
    , keccak256
    , functionSelector
    , lenUint256
    , encodeAbiValue
    , decodeAbiValue
    , encodeAbi'
    , encodeAbi
    , decodeAbi'
    , decodeAbi
    , eventParamValueIsHash256
    , decodeLog
    , decodeLogs
    , encodeEventFilter
    , topicNull
    , topicBool
    , topicInt
    , topicWord
    , topicAddr
    , topicBytesN
    , topicBs
    , topicStr
    ) where

import Control.Arrow ((&&&))
import Crypto.Hash (Digest(..), hash)
import Crypto.Hash.Algorithms (Keccak_256(..))
import Data.Aeson
import Data.Aeson.JsonUtils
import Data.Aeson.Types
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft, isRight, rights)
import Data.List (partition, sortBy)
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Network.Web3.Dapp.EthABI.AST
import Network.Web3.Dapp.EthABI.Types
import Network.Web3.Types
import Network.Web3.HexText

decodeSolcJson :: (Value -> Parser a) -> LBS.ByteString -> Either Text a
decodeSolcJson p bs =
  let ev = eitherDecode bs
  in case ev of
    Left e1 -> Left $ T.pack e1
    Right v ->
      let r = parse p v
      in case r of
        Error e2 -> Left $ T.pack e2
        Success cs -> Right cs

parseSolcAbi :: Value -> Parser [Contract]
parseSolcAbi (Object o) = parseListOfKeyObject o "contracts" parseContract

-- | Decodifica los contratos de la salida (JSON) del compilador solc.
decodeSolcAbi :: LBS.ByteString -> Either Text [Contract]
decodeSolcAbi = decodeSolcJson parseSolcAbi

-- | Decodifica el AST de los contratos de la salida (JSON) del compilador solc.
decodeSolcAst :: LBS.ByteString -> Either Text [SolcAST]
decodeSolcAst = decodeSolcJson parseSolcAst

-- | Decodifica el metadata.
decodeMetadata :: LBS.ByteString -> Either Text Metadata
decodeMetadata = decodeSolcJson parseJSON

keccak256 :: BS.ByteString -> BS.ByteString
keccak256 = BS.pack . BA.unpack . hashKeccak256
    where
        hashKeccak256 = hash :: BS.ByteString -> Digest Keccak_256

functionSelector :: (ToCanonical a) => a -> BS.ByteString
functionSelector = BS.take 4 . keccak256 . toCanonical

bytesContent :: Int -> BS.ByteString -> BS.ByteString
bytesContent sz bs =
  let len = BS.length bs
  in if len > sz
      then BS.drop (len - sz) bs
      else bs

showT :: (Show a) => a -> Text
showT = T.pack . show

eavError :: (Show o) => Text -> o -> Either Text BS.ByteString
eavError n o = Left $ n <> ": " <> showT o

zerosLen' n l = let m = l `mod` n in if m==0 then 0 else n-m
zerosLen n = zerosLen' n . BS.length
zerosLen256 = zerosLen lenUint256

align :: Word8 -> Int -> BS.ByteString -> (BS.ByteString,BS.ByteString)
align z n v = (v <> zeros, zeros <> v)
  where
    zeros = BS.replicate (zerosLen n v) z

align2hR = snd . align 48 2
align256b = align 0 lenUint256
align256bL = fst . align256b
align256bR = snd . align256b

lenUint256 = 32

lenHead (ty',tya',av') = if isDynamicType ty' tya' then lenUint256 else lenType ty' tya' av'
lenTail (ty',tya',av') = if isDynamicType ty' tya' then lenType ty' tya' av' else 0

lenHeads = sum . map lenHead
lenTails = sum . map lenTail

mkTuple n ty = TyTuple $ replicate n $ Param "" ty NoArray

lenType :: Type -> TypeArray -> AbiValue -> Int
lenType ty tya av = case tya of
  DynamicArray -> lenDynamicArray av
  FixedArray n -> lenFixedArray n av
  NoArray -> case ty of
    (TyTuple cs) -> lenTuple cs av
    (TyInt _ _) -> lenUint256
    TyAddress -> lenUint256
    TyBool -> lenUint256
    (TyFixed sg m n) -> lenUint256
    (TyBin sz) -> lenUint256
    TyBytes -> lenBytes av
    TyUtf8 -> lenString av
    TyFunction -> error "NoImpl"
  where
    lenDynamicArray (AVArray avs) = lenUint256 + lenType ty (FixedArray $ length avs) av
    lenDynamicArray _ = 0
    lenFixedArray n (AVArray avs) = lenType (mkTuple n ty) NoArray (AVTuple avs)
    lenFixedArray _ _ = 0
    lenBytes (AVBytes bs) = lenUint256 + BS.length bs + zerosLen256 bs
    lenBytes _ = 0
    lenString (AVString t) = lenType TyBytes NoArray (toAbiValue $ TE.encodeUtf8 t)
    lenString _ = 0
    lenTuple cs (AVTuple avs) =
      let td = zip3 (map abiParamType cs) (map abiParamTypeArray cs) avs
      in lenHeads td + lenTails td
    lenTuple _ _ = 0

-- | Codifica un valor según la especificación Ethereum ABI.
encodeAbiValue :: Type -> TypeArray -> AbiValue -> Either Text BS.ByteString
encodeAbiValue ty tya av = case tya of
  DynamicArray -> eavDynamicArray av
  FixedArray n -> eavFixedArray n av
  NoArray -> case ty of
    (TyTuple cs) -> eavTuple cs av
    (TyInt sg sz) -> eavInt sg sz av
    TyAddress -> eavAddr av
    TyBool -> eavBool av
    (TyFixed sg m n) -> eavFixed sg n av
    (TyBin sz) -> eavBin sz av
    TyBytes -> eavBytes av
    TyUtf8 -> eavString av
    TyFunction -> Left "encodeAbiValue: Function not implemmented!!!"
  where
    eavDynamicArray (AVArray a) = encodeAbiValue uint256 NoArray (toAbiValue $ length a)
                     `concatEavM` encodeAbiValue ty (FixedArray $ length a) av
    eavDynamicArray _ = eavError "eavDynamicArray: " av
    eavFixedArray n (AVArray avs) = encodeAbiValue (mkTuple n ty) NoArray (AVTuple avs)
    eavFixedArray _ _ = eavError "eavFixedArray" av
    eavTuple cs (AVTuple avs) =
      if length cs == length avs
        then
          let td = zip3 (map abiParamType cs) (map abiParamTypeArray cs) avs
          in eavComps td >>= \(hs,ts) -> return $ BS.concat hs <> BS.concat ts
        else Left $ "eavTuple: Different list length: " <> showT cs <> " " <> showT avs
    eavTuple _ _ = eavError "eavTuple" av
    eavComps td = foldl (eavCompE td) (Right ([],[])) td
    eavCompE td er tt = er >>= flip (eavComp td) tt
    eavComp td (hs,ts) (ty',tya',av') =
      let eh = if isDynamicType ty' tya'
                then encodeAbiValue uint256 NoArray
                      (toAbiValue $ lenHeads td + lenTails (take (length ts) td))
                else encodeAbiValue ty' tya' av'
          et = if isDynamicType ty' tya'
                then encodeAbiValue ty' tya' av'
                else return BS.empty
      in procEavM (\h t -> (hs ++ [h], ts ++ [t])) eh et
    eavInt sg sz (AVDec d) =
      let d2 = d `mod` (2^256)
      in Right $ align256bR $ fst $ B16.decode $ align2hR
               $ TE.encodeUtf8 $ stripHex $ toHex
               $ if d2>=0 then d2 else (2^256) + d2
    eavInt _ _ _ = eavError "eavInt" av
    eavAddr (AVAddr addr) = encodeAbiValue uint160 NoArray (AVDec $ fromHex $ getHexAddr addr)
    eavAddr _ = eavError "eavAddr" av
    eavBool (AVBool b) = encodeAbiValue uint8 NoArray (toAbiValue $ fromEnum b)
    eavBool _ = eavError "eavBool" av
    eavFixed sg n (AVFrac f) = encodeAbiValue (TyInt sg 256) NoArray (AVDec $ round $ f * 10^n)
    eavFixed _ _ _ = eavError "eavFixed" av
    eavBin sz (AVBytes bs) = return $ align256bL $ bytesContent sz bs
    eavBin _ _ = eavError "eavBin" av
    eavBytes (AVBytes bs) = encodeAbiValue uint256 NoArray (toAbiValue $ BS.length bs)
               `concatEavM` encodeAbiValue (TyBin $ BS.length bs) NoArray av
    eavBytes _ = eavError "eavBytes" av
    eavString (AVString t) = encodeAbiValue TyBytes NoArray (toAbiValue $ TE.encodeUtf8 t)
    eavString _ = eavError "eavString" av
    concatEavM = procEavM (<>)
    procEavM f ee1 ee2 = ee1 >>= \e1 -> ee2 >>= \e2 -> return $ f e1 e2

parseAbiValue :: Type -> TypeArray -> P.Parser AbiValue
parseAbiValue ty tya = case tya of
  DynamicArray -> davDynamicArray
  FixedArray n -> davFixedArray n
  NoArray -> case ty of
    (TyTuple cs) -> davTuple cs
    (TyInt sg _) -> davInt sg
    TyAddress -> davAddr
    TyBool -> davBool
    (TyFixed sg m n) -> davFixed sg n
    (TyBin sz) -> davBin sz
    TyBytes -> davBytes
    TyUtf8 -> davString
    TyFunction -> fail "parseAbiValue: Function not implemmented!!!"
  where
    davDynamicArray = parseAbiValue uint256 NoArray
                  >>= davFixedArray . fromIntegral . getAbiDec
    davFixedArray n = AVArray . getAbiTuple
                  <$> parseAbiValue (mkTuple n ty) NoArray
    davTuple cs = do
      let td = map (abiParamType &&& abiParamTypeArray) cs
      hs <- mapM davHead td
      ts <- mapM davTail td
      return $ AVTuple $ map getTupleComp $ zip3 td hs ts
    getTupleComp ((ty',tya'), h, t) = if isDynamicType ty' tya' then t else h
    davHead (ty',tya') =
      if isDynamicType ty' tya'
        then parseAbiValue uint256 NoArray
        else parseAbiValue ty' tya'
    davTail (ty',tya') =
      if isDynamicType ty' tya'
        then parseAbiValue ty' tya'
        else return (AVBytes BS.empty)
    davInt sg = do
      bs <- P.take lenUint256
      let i = fromHex $ joinHex
            $ T.dropWhile ('0'==) $ TE.decodeUtf8
            $ B16.encode bs
      return $ AVDec $ if sg && i > (2^255) then i - (2^256) else i
    davAddr = AVAddr . HexEthAddr . toHex . getAbiDec
          <$> parseAbiValue uint160 NoArray
    davBool = AVBool . toEnum . fromIntegral . getAbiDec
          <$> parseAbiValue uint8 NoArray
    davFixed sg n = AVFrac . (/(10^n)) . fromIntegral . getAbiDec
                <$> parseAbiValue (TyInt sg 256) NoArray
    davBin sz = AVBytes . BS.take sz <$> P.take (sz + zerosLen' lenUint256 sz)
    davBytes = parseAbiValue uint256 NoArray
           >>= davBin . fromIntegral . getAbiDec
    davString = AVString . TE.decodeUtf8 . getAbiBytes
            <$> parseAbiValue TyBytes NoArray

decodeAbiValueWithParser :: (Type -> TypeArray -> P.Parser AbiValue)
                         -> Type -> TypeArray -> HexData -> Either Text AbiValue
decodeAbiValueWithParser p ty tya =
  either (Left . ("decodeAbiValueWithParser: "<>) . T.pack) Right
    . P.parseOnly (p ty tya <* P.endOfInput)
    . fromHex

-- | Decodifica datos codificados según la especificación Ethereum ABI.
decodeAbiValue :: Type -> TypeArray -> HexData -> Either Text AbiValue
decodeAbiValue = decodeAbiValueWithParser parseAbiValue

outputsIsOneValueTuple :: [Param] -> (Bool,(Type,TypeArray))
outputsIsOneValueTuple os = case length os of
  1 -> let o = head os
           (ty,tya) = (abiParamType o, abiParamTypeArray o)
       in if isDynamicType ty tya
            then (True,(TyTuple os,NoArray))
            else (False,(ty,tya))
  _ -> (False,(TyTuple os,NoArray))

encodeAbiValueCall :: BS.ByteString -> [Param] -> AbiValue
                   -> Either Text AbiCallData
encodeAbiValueCall fs is av = AbiCallData fs
                          <$> encodeAbiValue (TyTuple is) NoArray av

-- | Codifica los parámetros de una función o contract según la
-- especificación Ethereum ABI. Ver `arg0`, `arg1` ...
encodeAbi' :: Interface -> AbiValue -> Either Text AbiCallData
encodeAbi' iface av = case iface of
  (IFunction f) -> encodeAbiValueCall (functionSelector f) (abiFuncInputs f) av
  (IConstructor f) -> encodeAbiValueCall BS.empty (abiConsInputs f) av
  _ -> Left $ "encodeAbi': " <> T.pack (show iface)

encodeAbi :: (AbiValueEncoding a)
          => Interface -> a -> Either Text AbiCallData
encodeAbi iface = encodeAbi' iface . toAbiValue

decodeAbiParams :: [Param] -> HexData -> Either Text AbiValue
decodeAbiParams ps hd =
  let (isOneValueTuple,(ty,tya)) = outputsIsOneValueTuple ps
      eav = decodeAbiValue ty tya hd
  in (if isOneValueTuple then head . getAbiTuple else id) <$> eav

-- | Decodifica datos codificados según la especificación Ethereum ABI.
decodeAbi' :: Interface -> HexData -> Either Text AbiValue
decodeAbi' iface hd = case iface of
  (IFunction f) -> decodeAbiParams (abiFuncOutputs f) hd
  _ -> Left $ "decodeAbi': " <> T.pack (show iface)

decodeAbi :: (AbiValueEncoding a)
          => Interface -> HexData -> Either Text a
decodeAbi iface bs = decodeAbi' iface bs >>= fromAbiValue

getEvents :: [Interface] -> [Event]
getEvents = map abiInterfaceEvent
          . filter isInterfaceEvent

getEvents' :: [Interface] -> ([Event],[Event])
getEvents' = partition abiEventAnonymous . getEvents

logTopic0 :: Event -> HexHash256
logTopic0 ev = if abiEventAnonymous ev
                then joinHex ""
                else toHex $ keccak256 $ toCanonical ev

-- | Decodifica los datos de un log. Devuelve `Nothing` si no encuentra la
-- interfaz del evento que decodifica los datos, sino devuelve el resultado
-- de la decodificación.
decodeEventLog :: [(HexHash256,Event)]    -- ^ Lista de eventos del `Contract` junto a su topic[0]
               -> [HexData] -- ^ Log topics
               -> HexData   -- ^ Datos del log
               -> DecodeLogResult
decodeEventLog evs tps lgd =
  if null tps
    then Nothing
    else
      let t0 = head tps
          evs' = filter ((t0==) . fst) evs
          nEvs' = length evs'
      in if nEvs' == 0
          then Nothing
          else Just $ if nEvs' == 1
            then decodeLogWithEvent (snd $ head evs') (tail tps) lgd
            else Left "decodeEventLog: More than one event matches topic 0"

fromUserArg :: AbiValue -> [AbiValue]
fromUserArg (AVTuple avs) = avs
fromUserArg av = [av]

toUserArg :: [AbiValue] -> AbiValue
toUserArg avs = if length avs == 1 then head avs else AVTuple avs

decodeLogWithEvent :: Event -> [HexData] -> HexData -> Either Text (Text,AbiValue)
decodeLogWithEvent ev tps lgd =
  let evps = abiEventInputs ev
      (ips,nips) = eventParams evps
      eiavs = decodeEventLogTopics ips tps
      eniavs = decodeEvenLogData nips lgd
  in eiavs >>= \iavs -> eniavs
           >>= \niavs -> (return . (,) (abiEventName ev) . toUserArg)
                            (foldTopicAvs iavs niavs evps)
  where
    foldTopicAvs :: [AbiValue] -> [AbiValue] -> [EventParam] -> [AbiValue]
    foldTopicAvs iavs niavs = reverse . fst . foldl topicAv ([],(iavs,niavs))
    topicAv (rtavs,(iavs,niavs)) evp =
      if abiEventParamIndexed evp
        then (head iavs:rtavs,(tail iavs,niavs))
        else (head niavs:rtavs,(iavs,tail niavs))

decodeEventLogTopics :: [Param] -> [HexData] -> Either Text [AbiValue]
decodeEventLogTopics cs tps =
  if length cs == length tps
    then
      let ds = zip3 (map abiParamType cs) (map abiParamTypeArray cs) tps
      in mapM (\(ty,tya,hd) -> decodeTopicAbiValue ty tya hd) ds
    else Left "decodeEventLogTopics: Number of indexed params not equeal to number of log topics"

topicValueIsHash256 :: Type -> TypeArray -> Bool
topicValueIsHash256 ty tya = case tya of
  DynamicArray -> True
  FixedArray _ -> True
  NoArray -> case ty of
    (TyTuple _) -> True
    TyInt{} -> False
    TyAddress -> False
    TyBool -> False
    TyFixed{} -> False
    TyBin{} -> False
    TyBytes -> True
    TyUtf8 -> True
    TyFunction -> False

eventParamValueIsHash256 :: Bool -> Param -> Bool
eventParamValueIsHash256 idx evp =
  idx && topicValueIsHash256 (abiParamType evp) (abiParamTypeArray evp)

parseTopicAbiValue :: Type -> TypeArray -> P.Parser AbiValue
parseTopicAbiValue ty tya =
  if topicValueIsHash256 ty tya
    then parseHash256
    else parseAbiValue ty tya
  where
    parseHash256 = AVString . toHex . getAbiBytes
               <$> parseAbiValue (TyBin lenUint256) NoArray

decodeTopicAbiValue :: Type -> TypeArray -> HexData -> Either Text AbiValue
decodeTopicAbiValue = decodeAbiValueWithParser parseTopicAbiValue

decodeEvenLogData :: [Param] -> HexData -> Either Text [AbiValue]
decodeEvenLogData cs hd = fromUserArg <$> decodeAbiParams cs hd

-- | Decodifica un log emitido por la llamada a un contract. Devuelve `Nothing`
-- si no encuentra el evento cuya información decodifica los valores. En caso
-- de encontrar más de un evento que decodifique los valores, o algún error
-- durante la decodificación se devuelve el error (`Left`).
-- Si se decodifica correctamente devuelve el nombre del evento y la tupla de
-- valores decodificados.
decodeLog :: [Interface] -> RpcEthLog -> DecodeLogResult
decodeLog ifs = head . decodeLogs ifs . (:[])

-- | Decodifica los logs emitidos por la llamada a un contract. Ver `decodeLog`.
decodeLogs :: [Interface] -> [RpcEthLog] -> [DecodeLogResult]
decodeLogs ifs logs =
  let (anonevs,evs) = getEvents' ifs
      evt0s = map logTopic0 evs
      evs' = zip evt0s evs
      logDats = map (logTopics &&& logData) $ sortBy (comparing logLogIndex) logs
      mdevs = map (uncurry (decodeEventLog evs')) logDats
      mdevLogDats = zip mdevs logDats
      noDecLogDats = map snd $ filter (isNothing . fst) mdevLogDats
  in if null noDecLogDats
    then mdevs
    else
      let ndsProc = map (\ndld ->
            let ndProc = map (\aev ->
                                uncurry (decodeLogWithEvent aev) ndld) anonevs
                ndProcOk = rights ndProc
                numOk = length ndProcOk
            in case numOk of
                0 -> Nothing
                1 -> Just $ Right $ head ndProcOk
                _ -> Just $ Left "decodeLogs: Too many anonymous events match log data" ) noDecLogDats
      in reverse $ fst $ foldl (\(rr,ndsP') mdev ->
        if isNothing mdev
          then (head ndsP':rr,tail ndsP')
          else (mdev:rr,ndsP') ) ([],ndsProc) mdevs

encodeTopicAbiValue :: Type -> TypeArray -> AbiValue -> Either Text BS.ByteString
encodeTopicAbiValue ty tya av = case tya of
  DynamicArray -> etavDynamicArray av
  FixedArray n -> etavFixedArray n av
  NoArray -> case ty of
    (TyTuple cs) -> etavTuple cs av
    TyInt{} -> encodeAbiValue ty tya av
    TyAddress -> encodeAbiValue ty tya av
    TyBool -> encodeAbiValue ty tya av
    TyFixed{} -> encodeAbiValue ty tya av
    TyBin{} -> encodeAbiValue ty tya av
    TyBytes -> etavBytes av
    TyUtf8 -> etavString av
    TyFunction -> encodeAbiValue ty tya av
  where
    etavTuple cs (AVTuple avs) =
      if length cs == length avs
        then
          let td = zip3 (map abiParamType cs) (map abiParamTypeArray cs) avs
          in BS.concat <$> mapM etavComp td
        else Left $ "etavTuple: Different list length: " <> showT cs <> " " <> showT avs
    etavTuple _ _ = eavError "eavTuple" av
    etavComp (ty',tya',av') = encodeTopicAbiValue ty' tya' av'
    etavDynamicArray (AVArray a) = encodeTopicAbiValue ty (FixedArray $ length a) av
    etavDynamicArray _ = eavError "etavDynamicArray: " av
    etavFixedArray n (AVArray avs) = encodeTopicAbiValue (mkTuple n ty) NoArray (AVTuple avs)
    etavFixedArray _ _ = eavError "etavFixedArray" av
    etavBytes (AVBytes bs) = return bs
    etavBytes _ = eavError "etavBytes" av
    etavString (AVString t) = encodeTopicAbiValue TyBytes NoArray (toAbiValue $ TE.encodeUtf8 t)
    etavString _ = eavError "etavString" av

encodeFilterWithEvent :: (HexHash256,Event)
                      -> [Maybe AbiValue]
                      -> Either Text [RpcEthFilterTopic]
encodeFilterWithEvent (t0,ev) mavs =
  let ips = eventIndexedParams $ abiEventInputs ev
      -- convertir los ips hash256 en av hash256 (bytestring)
      ipsmavs = zip ips mavs
      meeavs = map encodeTopicValue ipsmavs
      merreavs = filter (maybe False isLeft) meeavs
  in if null merreavs
      then
        let eavs = map (maybe EthFilterTopicNull
                              (either (error . show)
                                      (EthFilterTopic . toHex))) meeavs
        in Right $ map EthFilterTopicValue (topic0Value : eavs)
      else Left $ "Error encoding " <> T.pack (show mavs)
               <> ": " <> T.pack (show merreavs)
  where
    topic0Value = EthFilterTopic t0
    encodeTopicValue (ip,mav) = do
      let (ty,tya) = (abiParamType ip, abiParamTypeArray ip)
      if eventParamValueIsHash256 True ip
        then (fmap keccak256 . encodeTopicAbiValue ty tya) <$> mav
        else encodeAbiValue ty tya <$> mav

-- Asumo que los vs son compatibles con el tipo de los inputs del event
-- Cuando implemente busqueda de funciones por nombre y compatibilidad
-- de parametros -> aplicarlo aqui -> o sea no solo comparar el numero
-- de elementos, sino que son compatibles uno a uno
filterEvent :: Text -> [a] -> ([EventParam] -> [Param]) -> Event -> Bool
filterEvent evName vs fp ev =
  (evName == abiEventName ev)
      && (length vs == length (fp $ abiEventInputs ev))

-- | Codifica los filter topics de un evento usando la interfaz del
-- `Contract`. Devuelve `Nothing` si no encutra el `Event` que codifica
-- la información pasada. Devuelve `Left` si encuentra varios eventos
-- que codifican la información pasada. Sino devuelve los filter topics.
encodeEventFilter :: [Interface]        -- ^ Interfaz del `Contract`
                  -> Text               -- ^ Nombre del `Event`
                  -> [Maybe AbiValue]   -- ^ Log topics a codificar
                  -> Maybe (Either Text [RpcEthFilterTopic])
encodeEventFilter ifs evName mavs =
  let evs = getEvents ifs
      evt0s = map logTopic0 evs
      t0sevs = zip evt0s evs
      t0sevs' = filter (filterEvent evName mavs eventIndexedParams . snd) t0sevs
      esft = map (`encodeFilterWithEvent` mavs) t0sevs'
      t0sevsesft = zip t0sevs' esft
      t0sevsfts = filter (isRight . snd) t0sevsesft
      lenEsft = length t0sevsesft
      lenFts = length t0sevsfts
  in case lenFts of
      0 -> case lenEsft of
            0 -> Nothing
            1 -> Just (snd $ head t0sevsesft)
            _ -> Just (Left $ "encodeEventFilter: Too many events match filter: "
                           <> T.pack (show t0sevsesft))
      1 -> Just (snd $ head t0sevsfts)
      _ -> Just (Left $ "encodeEventFilter: Too many events encode filter: "
                     <> T.pack (show t0sevsfts))

topicValue :: (AbiValueEncoding a) => Type -> TypeArray -> a -> RpcEthFilterTopic
topicValue ty tya = EthFilterTopicValue . EthFilterTopic
                  . toHex . fromRight . encodeAbiValue ty tya . toAbiValue

topicInt :: (Integral a) => a -> RpcEthFilterTopic
topicInt = topicValue int256 NoArray . toInteger

topicWord :: (Integral a) => a -> RpcEthFilterTopic
topicWord = topicValue uint256 NoArray . toInteger

topicBool :: Bool -> RpcEthFilterTopic
topicBool = topicValue TyBool NoArray

topicAddr :: HexEthAddr -> RpcEthFilterTopic
topicAddr = topicValue TyAddress NoArray

topicStr :: Text -> RpcEthFilterTopic
topicStr = topicValue TyUtf8 NoArray

topicBs :: BS.ByteString -> RpcEthFilterTopic
topicBs = topicValue TyBytes NoArray

topicBytesN :: Int -> BS.ByteString -> RpcEthFilterTopic
topicBytesN n = topicValue (TyBin n) NoArray

topicNull :: RpcEthFilterTopic
topicNull = EthFilterTopicValue EthFilterTopicNull

