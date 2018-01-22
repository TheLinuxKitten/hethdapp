{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

module Network.Web3.Dapp.EthABI.Types
  ( Type(..)
  , uint8
  , uint160
  , uint256
  , int256
  , TypeArray(..)
  , isDynamicType
  , Param(..)
  , StateMutability(..)
  , Function(..)
  , Constructor(..)
  , Fallback(..)
  , EventParam(..)
  , eventParams
  , eventIndexedParams
  , eventNotIndexedParams
  , Event(..)
  , Interface(..)
  , isInterfaceConstructor
  , isInterfaceFunction
  , isInterfaceEvent
  , Contract(..)
  , parseContract
  , lookupInterfaceConstructor
  , lookupConstructor
  , lookupInterfaceFunction
  , lookupFunction
  , ToCanonical(..)
  , AbiValue(..)
  , AbiValueEncoding(..)
  , arg0
  , arg1
  , arg2
  , arg3
  , arg4
  , arg5
  , arg6
  , arg7
  , arg8
  , arg9
  , arg10
  , toMaybeAbiValue
  , toMaybeAbiValue1
  , toMaybeAbiValue2
  , toMaybeAbiValue3
  , toMaybeAbiValue4
  , toMaybeAbiValue5
  , toMaybeAbiValue6
  , toMaybeAbiValue7
  , toMaybeAbiValue8
  , toMaybeAbiValue9
  , toMaybeAbiValue10
  , AbiCallData(..)
  , abiCallDataToHexText
  , fromRight
  , DecodeLogResult(..)
  , maybeDecodeLogResult
  , fromDecodeLogResult
  , logEventName
  , logEventValue
  , FromLogEvent(..)
  , NullEvent(..)
  , null_decode_log
  , ToEventFilter(..)
  , CompilationTarget(..)
  , Optimizer(..)
  , Settings(..)
  , Source(..)
  , Output(..)
  , ProgLanguage(..)
  , Compiler(..)
  , Metadata(..)
  ) where

import Control.Arrow ((***),(&&&))
import Data.Aeson
import Data.Aeson.JsonUtils
import Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Char (isDigit)
import qualified Data.HashMap.Lazy as LHM
import Data.Int
import Data.List (partition)
import Data.Maybe (fromJust, listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word
import Instances.TH.Lift
import qualified Language.Haskell.TH.Syntax as THS
import Network.Web3.Types
import Network.Web3.HexText

data Type =
      TyTuple { abiTupleComponents :: [Param] }
    | TyInt { abiIntSigned :: Bool, abiIntSize :: Int }
    | TyAddress
    | TyBool
    | TyFixed { abiFixedSigned :: Bool, abiFixedM :: Int, abiFixedN :: Int }
    | TyBin { abiBytesSize :: Int }
    | TyBytes
    | TyUtf8
    | TyFunction
    deriving (Eq, THS.Lift, Show)

uint8 = TyInt False 8
uint160 = TyInt False 160
uint256 = TyInt False 256
int256 = TyInt True 256

data TypeArray = NoArray
               | FixedArray Int
               | DynamicArray
               deriving (Eq, THS.Lift, Show)

data Param = Param
    { abiParamName :: Text
    , abiParamType :: Type
    , abiParamTypeArray :: TypeArray
    } deriving (Eq, THS.Lift, Show)

parseType :: Text -> Maybe [Param] -> (Type,TypeArray)
parseType sty mcs
    | "uint" `T.isPrefixOf` sty = parseInt False (T.drop 4 sty)
    | "int" `T.isPrefixOf` sty = parseInt True (T.drop 3 sty)
    | "address" `T.isPrefixOf` sty = parseAddress (T.drop 7 sty)
    | "bool" `T.isPrefixOf` sty = parseBool (T.drop 4 sty)
    | "ufixed" `T.isPrefixOf` sty = parseFixed False (T.drop 6 sty)
    | "fixed" `T.isPrefixOf` sty = parseFixed True (T.drop 5 sty)
    | "bytes" `T.isPrefixOf` sty = parseBytes (T.drop 5 sty)
    | "string" `T.isPrefixOf` sty = parseString (T.drop 6 sty)
    | "tuple" `T.isPrefixOf` sty = parseTuple (T.drop 5 sty) (fromJust mcs)
    | "function" `T.isPrefixOf` sty = parseFunction (T.drop 8 sty)
    where
        parseInt sg sty' =
            let (sm,stya) = popNum sty'
                m = if sm=="" then 256 else readT sm
            in (TyInt sg m, parseTypeArray stya)
        parseAddress sty' = (TyAddress, parseTypeArray sty')
        parseBool sty' = (TyBool, parseTypeArray sty')
        parseFixed sg sty'
            | sty' == "" = (TyFixed sg 128 19, parseTypeArray sty')
            | otherwise = let (sm,stya1) = popNum sty'
                              (sn,stya2) = popNum (T.tail stya1)
                          in (TyFixed sg (readT sm) (readT sn), parseTypeArray stya2)
        parseBytes sty' =
            let (sm,stya) = popNum sty'
            in if sm == ""
                then (TyBytes, parseTypeArray stya)
                else (TyBin $ readT sm, parseTypeArray stya)
        parseString sty' = (TyUtf8, parseTypeArray sty')
        parseTuple sty' cs = (TyTuple cs, parseTypeArray sty')
        parseFunction sty' = (TyFunction, parseTypeArray sty')
        parseTypeArray stya
            | stya == "" = NoArray
            | stya == "[]" = DynamicArray
            | otherwise = FixedArray $ readT $ T.init $ T.tail stya
        popNum = T.span isDigit
        readT = read . T.unpack

instance FromJSON Param where
    parseJSON (Object o) = do
        n <- o .: "name"
        sty <- o .: "type"
        mcs <- o .:? "components"
        let (ty,tya) = parseType sty mcs
        return $ Param n ty tya

data StateMutability = SMPure
                     | SMView
                     | SMNonPayable
                     | SMPayable
                     deriving (Eq, THS.Lift, Show)

instance FromJSON StateMutability where
    parseJSON (String s) = return $ case s of
        "pure" -> SMPure
        "view" -> SMView
        "nonpayable" -> SMNonPayable
        "payable" -> SMPayable

data Function = Function
    { abiFuncName :: Text
    , abiFuncInputs :: [Param]
    , abiFuncOutputs :: [Param]
    , abiFuncPayable :: Bool
    , abiFuncStateMutability :: StateMutability
    , abiFuncConstant :: Bool
    } deriving (Eq, THS.Lift, Show)

instance FromJSON Function where
    parseJSON (Object o) = Function
                       <$> o .: "name"
                       <*> o .: "inputs"
                       <*> o .:? "outputs" .!= []
                       <*> o .:? "payable" .!= False
                       <*> o .: "stateMutability"
                       <*> o .: "constant"

data Constructor = Constructor
    { abiConsInputs :: [Param]
    , abiConsPayable :: Bool
    , abiConsStateMutability :: StateMutability
    , abiConsConstant :: Bool
    } deriving (Eq, THS.Lift, Show)

instance FromJSON Constructor where
    parseJSON (Object o) = Constructor
                       <$> o .: "inputs"
                       <*> o .:? "payable" .!= False
                       <*> o .: "stateMutability"
                       <*> o .:? "constant" .!= False

data Fallback = Fallback
    { abiFallPayable :: Bool
    , abiFallStateMutability :: StateMutability
    , abiFallConstant :: Bool
    } deriving (Eq, THS.Lift, Show)

instance FromJSON Fallback where
    parseJSON (Object o) = Fallback
                       <$> o .:? "payable" .!= False
                       <*> o .: "stateMutability"
                       <*> o .: "constant"

data EventParam = EventParam
    { abiEventParam :: Param
    , abiEventParamIndexed :: Bool
    } deriving (Eq, THS.Lift, Show)

instance FromJSON EventParam where
    parseJSON v@(Object o) = EventParam
                         <$> parseJSON v
                         <*> o .: "indexed"

data Event = Event
    { abiEventName :: Text
    , abiEventInputs :: [EventParam]
    , abiEventAnonymous :: Bool
    } deriving (Eq, THS.Lift, Show)

instance FromJSON Event where
    parseJSON (Object o) = Event
                       <$> o .: "name"
                       <*> o .: "inputs"
                       <*> o .: "anonymous"

data Interface = IFunction { abiInterfaceFunction :: Function }
               | IConstructor { abiInterfaceConstructor :: Constructor }
               | IFallback { abiInterfaceFallback :: Fallback }
               | IEvent { abiInterfaceEvent :: Event }
               deriving (Eq, THS.Lift, Show)

isInterfaceConstructor :: Interface -> Bool
isInterfaceConstructor iface = case iface of
  IConstructor _ -> True
  _ -> False

isInterfaceFunction :: Interface -> Bool
isInterfaceFunction iface = case iface of
  IFunction _ -> True
  _ -> False

isInterfaceEvent :: Interface -> Bool
isInterfaceEvent iface = case iface of
  IEvent _ -> True
  _ -> False

instance FromJSON Interface where
    parseJSON v@(Object o) = do
        ty <- o .:? "type" .!= "function"
        case (ty :: Text) of
            "function" -> IFunction <$> parseJSON v
            "constructor" -> IConstructor <$> parseJSON v
            "fallback" -> IFallback <$> parseJSON v
            "event" -> IEvent <$> parseJSON v

data Contract = Contract
    { abiContractName :: Text
    , abiContractPath :: FilePath
    , abiContractAbi :: [Interface]
    , abiContractBin :: Maybe HexData
    , abiContractBinRuntime :: HexData
    , abiContractMetadata :: LBS.ByteString
    } deriving (Eq, THS.Lift, Show)

parseContract :: Text -> Value -> Parser Contract
parseContract n (Object o) = Contract (snd $ spanName n) (fst $ spanName n)
                         <$> (decodeJsonAbi <$> o .: "abi")
                         <*> (maybe Nothing (Just . joinHex) <$> o .:? "bin")
                         <*> (joinHex <$> o .: "bin-runtime")
                         <*> (LBS.fromStrict . TE.encodeUtf8 <$> o .: "metadata")
  where
    spanName t = let [p,nom] = T.split (':'==) t in (T.unpack p, nom)
    decodeJsonAbi = fromJust . decode . LC8.pack . T.unpack

getConstructors :: Contract -> [Interface]
getConstructors = filter isInterfaceConstructor . abiContractAbi

getFunctions :: Contract -> [Interface]
getFunctions = filter isInterfaceFunction . abiContractAbi

-- | Devuelve el constructor del `Contract`, si existe.
lookupInterfaceConstructor :: Contract -> Maybe Interface
lookupInterfaceConstructor = listToMaybe . getConstructors

-- | Devuelve el constructor del `Contract`, si existe.
lookupConstructor :: Contract -> Maybe Constructor
lookupConstructor c = abiInterfaceConstructor <$> lookupInterfaceConstructor c

-- | Busca función por nombre y la devuelve si existe.
lookupInterfaceFunction :: Text -> Contract -> Maybe Interface
lookupInterfaceFunction n = listToMaybe 
                          . filter ((n==) . abiFuncName . abiInterfaceFunction)
                          . getFunctions

-- | Busca función por nombre y la devuelve si existe.
lookupFunction :: Text -> Contract -> Maybe Function
lookupFunction n c = abiInterfaceFunction <$> lookupInterfaceFunction n c

class ToCanonical a where
    toCanonical :: a -> BS.ByteString

signedTC sg = if sg then "" else "u"
sizeTC = C8.pack . show

instance ToCanonical Type where
    toCanonical (TyTuple cs) = funcToCanonical "" cs
    toCanonical (TyInt sg sz) = signedTC sg <> "int" <> sizeTC sz
    toCanonical TyAddress = "address"
    toCanonical TyBool = "bool"
    toCanonical (TyFixed sg m n) = signedTC sg <> "fixed" <> sizeTC m <> "x" <> sizeTC n
    toCanonical (TyBin sz) = "bytes" <> sizeTC sz
    toCanonical TyBytes = "bytes"
    toCanonical TyUtf8 = "string"
    toCanonical TyFunction = toCanonical (TyBin 24)

instance ToCanonical TypeArray where
    toCanonical NoArray = ""
    toCanonical (FixedArray k) = "[" <> C8.pack (show k) <> "]"
    toCanonical DynamicArray = "[]"

instance ToCanonical Param where
    toCanonical (Param _ ty tya) = toCanonical ty <> toCanonical tya

instance ToCanonical Function where
    toCanonical (Function n is _ _ _ _) = funcToCanonical n is

getEventParams = map abiEventParam

eventParams :: [EventParam] -> ([Param],[Param])
eventParams = (getEventParams *** getEventParams) . partition abiEventParamIndexed

eventIndexedParams = fst . eventParams
eventNotIndexedParams = snd . eventParams

instance ToCanonical Event where
    toCanonical (Event _ _ True) = BS.empty
    toCanonical (Event n is False) = funcToCanonical n (getEventParams is)

funcToCanonical :: Text -> [Param] -> BS.ByteString
funcToCanonical n ps = C8.pack (T.unpack n) <> "(" <> paramsToCanonical ps <> ")"
    where
        paramsToCanonical = C8.intercalate "," . map toCanonical

data AbiValue =
      AVDec { getAbiDec :: Integer }
    | AVFrac { getAbiFrac :: Double }
    | AVBool { getAbiBool :: Bool }
    | AVAddr { getAbiAddr :: HexEthAddr }
    | AVBytes { getAbiBytes :: BS.ByteString }
    | AVString { getAbiString :: Text }
    | AVArray { getAbiArray :: [AbiValue] }
    | AVTuple { getAbiTuple :: [AbiValue] }
    deriving (Eq, Show)

class AbiValueEncoding a where
    toAbiValue :: a -> AbiValue
    fromAbiValue :: AbiValue -> Either Text a

fromAbiErr :: AbiValueEncoding a => Text -> AbiValue -> Either Text a
fromAbiErr nt av = Left $ "No se puede convertir "
                       <> T.pack (show av) <> " a " <> nt

instance AbiValueEncoding AbiValue where
    toAbiValue = id
    fromAbiValue = Right

instance AbiValueEncoding Word where
    toAbiValue = AVDec . toInteger
    fromAbiValue (AVDec d) = Right $ fromIntegral d
    fromAbiValue av = fromAbiErr "Word" av

instance AbiValueEncoding Word8 where
    toAbiValue = AVDec . toInteger
    fromAbiValue (AVDec d) = Right $ fromIntegral d
    fromAbiValue av = fromAbiErr "Word8" av

instance AbiValueEncoding Word16 where
    toAbiValue = AVDec . toInteger
    fromAbiValue (AVDec d) = Right $ fromIntegral d
    fromAbiValue av = fromAbiErr "Word16" av

instance AbiValueEncoding Word32 where
    toAbiValue = AVDec . toInteger
    fromAbiValue (AVDec d) = Right $ fromIntegral d
    fromAbiValue av = fromAbiErr "Word32" av

instance AbiValueEncoding Word64 where
    toAbiValue = AVDec . toInteger
    fromAbiValue (AVDec d) = Right $ fromIntegral d
    fromAbiValue av = fromAbiErr "Word64" av

instance AbiValueEncoding Int where
    toAbiValue = AVDec . toInteger
    fromAbiValue (AVDec d) = Right $ fromIntegral d
    fromAbiValue av = fromAbiErr "Int" av

instance AbiValueEncoding Int8 where
    toAbiValue = AVDec . toInteger
    fromAbiValue (AVDec d) = Right $ fromIntegral d
    fromAbiValue av = fromAbiErr "Int8" av

instance AbiValueEncoding Int16 where
    toAbiValue = AVDec . toInteger
    fromAbiValue (AVDec d) = Right $ fromIntegral d
    fromAbiValue av = fromAbiErr "Int16" av

instance AbiValueEncoding Int32 where
    toAbiValue = AVDec . toInteger
    fromAbiValue (AVDec d) = Right $ fromIntegral d
    fromAbiValue av = fromAbiErr "Int32" av

instance AbiValueEncoding Int64 where
    toAbiValue = AVDec . toInteger
    fromAbiValue (AVDec d) = Right $ fromIntegral d
    fromAbiValue av = fromAbiErr "Int64" av

instance AbiValueEncoding Integer where
    toAbiValue = AVDec
    fromAbiValue (AVDec d) = Right d
    fromAbiValue av = fromAbiErr "Integer" av

instance AbiValueEncoding Double where
    toAbiValue = AVFrac
    fromAbiValue (AVFrac f) = Right f
    fromAbiValue av = fromAbiErr "Double" av

instance AbiValueEncoding Bool where
    toAbiValue = AVBool
    fromAbiValue (AVBool b) = Right b
    fromAbiValue av = fromAbiErr "Bool" av

instance AbiValueEncoding HexEthAddr where
    toAbiValue = AVAddr
    fromAbiValue (AVAddr ha) = Right ha
    fromAbiValue av = fromAbiErr "HexEthAddr" av

instance AbiValueEncoding BS.ByteString where
    toAbiValue = AVBytes
    fromAbiValue (AVBytes bs) = Right bs
    fromAbiValue av = fromAbiErr "ByteString" av

instance AbiValueEncoding Text where
    toAbiValue = AVString
    fromAbiValue (AVString t) = Right t
    fromAbiValue av = fromAbiErr "Text" av

instance AbiValueEncoding a => AbiValueEncoding [a] where
    toAbiValue = AVArray . map toAbiValue
    fromAbiValue (AVArray ar) = mapM fromAbiValue ar
    fromAbiValue av = fromAbiErr "[a]" av

instance (AbiValueEncoding a, AbiValueEncoding b) => AbiValueEncoding (a,b) where
    toAbiValue (a,b) = AVTuple [toAbiValue a, toAbiValue b]
    fromAbiValue (AVTuple [a,b]) = fromAbiValue a
                               >>= \a' -> fromAbiValue b
                               >>= \b' -> return (a',b')
    fromAbiValue av = fromAbiErr "(a,b)" av

instance (AbiValueEncoding a, AbiValueEncoding b, AbiValueEncoding c) => AbiValueEncoding (a,b,c) where
    toAbiValue (a,b,c) = AVTuple [toAbiValue a, toAbiValue b, toAbiValue c]
    fromAbiValue (AVTuple [a,b,c]) = fromAbiValue a
                                 >>= \a' -> fromAbiValue b
                                 >>= \b' -> fromAbiValue c
                                 >>= \c' -> return (a',b',c')
    fromAbiValue av = fromAbiErr "(a,b,c)" av

instance (AbiValueEncoding a, AbiValueEncoding b, AbiValueEncoding c, AbiValueEncoding d) => AbiValueEncoding (a,b,c,d) where
    toAbiValue (a,b,c,d) = AVTuple [toAbiValue a, toAbiValue b, toAbiValue c, toAbiValue d]
    fromAbiValue (AVTuple [a,b,c,d]) = fromAbiValue a
                                   >>= \a' -> fromAbiValue b
                                   >>= \b' -> fromAbiValue c
                                   >>= \c' -> fromAbiValue d
                                   >>= \d' -> return (a',b',c',d')
    fromAbiValue av = fromAbiErr "(a,b,c,d)" av

instance (AbiValueEncoding a, AbiValueEncoding b, AbiValueEncoding c, AbiValueEncoding d, AbiValueEncoding e) => AbiValueEncoding (a,b,c,d,e) where
    toAbiValue (a,b,c,d,e) = AVTuple [toAbiValue a, toAbiValue b, toAbiValue c, toAbiValue d, toAbiValue e]
    fromAbiValue (AVTuple [a,b,c,d,e]) = fromAbiValue a
                                     >>= \a' -> fromAbiValue b
                                     >>= \b' -> fromAbiValue c
                                     >>= \c' -> fromAbiValue d
                                     >>= \d' -> fromAbiValue e
                                     >>= \e' -> return (a',b',c',d',e')
    fromAbiValue av = fromAbiErr "(a,b,c,d,e)" av

instance (AbiValueEncoding a, AbiValueEncoding b, AbiValueEncoding c, AbiValueEncoding d, AbiValueEncoding e, AbiValueEncoding f) => AbiValueEncoding (a,b,c,d,e,f) where
    toAbiValue (a,b,c,d,e,f) = AVTuple [toAbiValue a, toAbiValue b, toAbiValue c, toAbiValue d, toAbiValue e, toAbiValue f]
    fromAbiValue (AVTuple [a,b,c,d,e,f]) = fromAbiValue a
                                       >>= \a' -> fromAbiValue b
                                       >>= \b' -> fromAbiValue c
                                       >>= \c' -> fromAbiValue d
                                       >>= \d' -> fromAbiValue e
                                       >>= \e' -> fromAbiValue f
                                       >>= \f' -> return (a',b',c',d',e',f')
    fromAbiValue av = fromAbiErr "(a,b,c,d,e,f)" av

instance (AbiValueEncoding a, AbiValueEncoding b, AbiValueEncoding c, AbiValueEncoding d, AbiValueEncoding e, AbiValueEncoding f, AbiValueEncoding g) => AbiValueEncoding (a,b,c,d,e,f,g) where
    toAbiValue (a,b,c,d,e,f,g) = AVTuple [toAbiValue a, toAbiValue b, toAbiValue c, toAbiValue d, toAbiValue e, toAbiValue f, toAbiValue g]
    fromAbiValue (AVTuple [a,b,c,d,e,f,g]) = fromAbiValue a
                                         >>= \a' -> fromAbiValue b
                                         >>= \b' -> fromAbiValue c
                                         >>= \c' -> fromAbiValue d
                                         >>= \d' -> fromAbiValue e
                                         >>= \e' -> fromAbiValue f
                                         >>= \f' -> fromAbiValue g
                                         >>= \g' -> return (a',b',c',d',e',f',g')
    fromAbiValue av = fromAbiErr "(a,b,c,d,e,f,g)" av

instance (AbiValueEncoding a, AbiValueEncoding b, AbiValueEncoding c, AbiValueEncoding d, AbiValueEncoding e, AbiValueEncoding f, AbiValueEncoding g, AbiValueEncoding h) => AbiValueEncoding (a,b,c,d,e,f,g,h) where
    toAbiValue (a,b,c,d,e,f,g,h) = AVTuple [toAbiValue a, toAbiValue b, toAbiValue c, toAbiValue d, toAbiValue e, toAbiValue f, toAbiValue g, toAbiValue h]
    fromAbiValue (AVTuple [a,b,c,d,e,f,g,h]) = fromAbiValue a
                                           >>= \a' -> fromAbiValue b
                                           >>= \b' -> fromAbiValue c
                                           >>= \c' -> fromAbiValue d
                                           >>= \d' -> fromAbiValue e
                                           >>= \e' -> fromAbiValue f
                                           >>= \f' -> fromAbiValue g
                                           >>= \g' -> fromAbiValue h
                                           >>= \h' -> return (a',b',c',d',e',f',g',h')
    fromAbiValue av = fromAbiErr "(a,b,c,d,e,f,g,h)" av

instance (AbiValueEncoding a, AbiValueEncoding b, AbiValueEncoding c, AbiValueEncoding d, AbiValueEncoding e, AbiValueEncoding f, AbiValueEncoding g, AbiValueEncoding h, AbiValueEncoding i) => AbiValueEncoding (a,b,c,d,e,f,g,h,i) where
    toAbiValue (a,b,c,d,e,f,g,h,i) = AVTuple [toAbiValue a, toAbiValue b, toAbiValue c, toAbiValue d, toAbiValue e, toAbiValue f, toAbiValue g, toAbiValue h, toAbiValue i]
    fromAbiValue (AVTuple [a,b,c,d,e,f,g,h,i]) = fromAbiValue a
                                             >>= \a' -> fromAbiValue b
                                             >>= \b' -> fromAbiValue c
                                             >>= \c' -> fromAbiValue d
                                             >>= \d' -> fromAbiValue e
                                             >>= \e' -> fromAbiValue f
                                             >>= \f' -> fromAbiValue g
                                             >>= \g' -> fromAbiValue h
                                             >>= \h' -> fromAbiValue i
                                             >>= \i' -> return (a',b',c',d',e',f',g',h',i')
    fromAbiValue av = fromAbiErr "(a,b,c,d,e,f,g,h,i)" av

instance (AbiValueEncoding a, AbiValueEncoding b, AbiValueEncoding c, AbiValueEncoding d, AbiValueEncoding e, AbiValueEncoding f, AbiValueEncoding g, AbiValueEncoding h, AbiValueEncoding i, AbiValueEncoding j) => AbiValueEncoding (a,b,c,d,e,f,g,h,i,j) where
    toAbiValue (a,b,c,d,e,f,g,h,i,j) = AVTuple [toAbiValue a, toAbiValue b, toAbiValue c, toAbiValue d, toAbiValue e, toAbiValue f, toAbiValue g, toAbiValue h, toAbiValue i, toAbiValue j]
    fromAbiValue (AVTuple [a,b,c,d,e,f,g,h,i,j]) = fromAbiValue a
                                               >>= \a' -> fromAbiValue b
                                               >>= \b' -> fromAbiValue c
                                               >>= \c' -> fromAbiValue d
                                               >>= \d' -> fromAbiValue e
                                               >>= \e' -> fromAbiValue f
                                               >>= \f' -> fromAbiValue g
                                               >>= \g' -> fromAbiValue h
                                               >>= \h' -> fromAbiValue i
                                               >>= \i' -> fromAbiValue j
                                               >>= \j' -> return (a',b',c',d',e',f',g',h',i',j')
    fromAbiValue av = fromAbiErr "(a,b,c,d,e,f,g,h,i,j)" av

-- | Funciones para codificar parámetros. Tanto en la codificación, como
-- en la decodificación, un solo valor se procesa directamente, mientras que
-- cero o más de un valor se trata como una tupla (`AVTuple`).

arg0 :: AbiValue
arg0 = AVTuple []

arg1 :: (AbiValueEncoding a) => a -> AbiValue
arg1 a = AVTuple [toAbiValue a]

arg2 :: (AbiValueEncoding a, AbiValueEncoding b) => a -> b -> AbiValue
arg2 a b = AVTuple [toAbiValue a, toAbiValue b]

arg3 :: (AbiValueEncoding a, AbiValueEncoding b, AbiValueEncoding c) => a -> b -> c -> AbiValue
arg3 a b c = AVTuple [toAbiValue a, toAbiValue b, toAbiValue c]

arg4 :: (AbiValueEncoding a, AbiValueEncoding b, AbiValueEncoding c, AbiValueEncoding d) => a -> b -> c -> d -> AbiValue
arg4 a b c d = AVTuple [toAbiValue a, toAbiValue b, toAbiValue c, toAbiValue d]

arg5 :: (AbiValueEncoding a, AbiValueEncoding b, AbiValueEncoding c, AbiValueEncoding d, AbiValueEncoding e) => a -> b -> c -> d -> e -> AbiValue
arg5 a b c d e = AVTuple [toAbiValue a, toAbiValue b, toAbiValue c, toAbiValue d, toAbiValue e]

arg6 :: (AbiValueEncoding a, AbiValueEncoding b, AbiValueEncoding c, AbiValueEncoding d, AbiValueEncoding e, AbiValueEncoding f) => a -> b -> c -> d -> e -> f -> AbiValue
arg6 a b c d e f = AVTuple [toAbiValue a, toAbiValue b, toAbiValue c, toAbiValue d, toAbiValue e, toAbiValue f]

arg7 :: (AbiValueEncoding a, AbiValueEncoding b, AbiValueEncoding c, AbiValueEncoding d, AbiValueEncoding e, AbiValueEncoding f, AbiValueEncoding g) => a -> b -> c -> d -> e -> f -> g -> AbiValue
arg7 a b c d e f g = AVTuple [toAbiValue a, toAbiValue b, toAbiValue c, toAbiValue d, toAbiValue e, toAbiValue f, toAbiValue g]

arg8 :: (AbiValueEncoding a, AbiValueEncoding b, AbiValueEncoding c, AbiValueEncoding d, AbiValueEncoding e, AbiValueEncoding f, AbiValueEncoding g, AbiValueEncoding h) => a -> b -> c -> d -> e -> f -> g -> h -> AbiValue
arg8 a b c d e f g h = AVTuple [toAbiValue a, toAbiValue b, toAbiValue c, toAbiValue d, toAbiValue e, toAbiValue f, toAbiValue g, toAbiValue h]

arg9 :: (AbiValueEncoding a, AbiValueEncoding b, AbiValueEncoding c, AbiValueEncoding d, AbiValueEncoding e, AbiValueEncoding f, AbiValueEncoding g, AbiValueEncoding h, AbiValueEncoding i) => a -> b -> c -> d -> e -> f -> g -> h -> i -> AbiValue
arg9 a b c d e f g h i = AVTuple [toAbiValue a, toAbiValue b, toAbiValue c, toAbiValue d, toAbiValue e, toAbiValue f, toAbiValue g, toAbiValue h, toAbiValue i]

arg10 :: (AbiValueEncoding a, AbiValueEncoding b, AbiValueEncoding c, AbiValueEncoding d, AbiValueEncoding e, AbiValueEncoding f, AbiValueEncoding g, AbiValueEncoding h, AbiValueEncoding i, AbiValueEncoding j) => a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> AbiValue
arg10 a b c d e f g h i j = AVTuple [toAbiValue a, toAbiValue b, toAbiValue c, toAbiValue d, toAbiValue e, toAbiValue f, toAbiValue g, toAbiValue h, toAbiValue i, toAbiValue j]

toMaybeAbiValue :: (AbiValueEncoding a) => Maybe a -> Maybe AbiValue
toMaybeAbiValue m = toAbiValue <$> m

toMaybeAbiValue1 :: (AbiValueEncoding a) => Maybe a -> [Maybe AbiValue]
toMaybeAbiValue1 a = [toMaybeAbiValue a]

toMaybeAbiValue2 :: (AbiValueEncoding a, AbiValueEncoding b) => (Maybe a, Maybe b) -> [Maybe AbiValue]
toMaybeAbiValue2 (a, b) = [toMaybeAbiValue a, toMaybeAbiValue b]

toMaybeAbiValue3 :: (AbiValueEncoding a, AbiValueEncoding b, AbiValueEncoding c) => (Maybe a, Maybe b, Maybe c) -> [Maybe AbiValue]
toMaybeAbiValue3 (a, b, c) = [toMaybeAbiValue a, toMaybeAbiValue b, toMaybeAbiValue c]

toMaybeAbiValue4 :: (AbiValueEncoding a, AbiValueEncoding b, AbiValueEncoding c, AbiValueEncoding d) => (Maybe a, Maybe b, Maybe c, Maybe d) -> [Maybe AbiValue]
toMaybeAbiValue4 (a, b, c, d) = [toMaybeAbiValue a, toMaybeAbiValue b, toMaybeAbiValue c, toMaybeAbiValue d]

toMaybeAbiValue5 :: (AbiValueEncoding a, AbiValueEncoding b, AbiValueEncoding c, AbiValueEncoding d, AbiValueEncoding e) => (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e) -> [Maybe AbiValue]
toMaybeAbiValue5 (a, b, c, d, e) = [toMaybeAbiValue a, toMaybeAbiValue b, toMaybeAbiValue c, toMaybeAbiValue d, toMaybeAbiValue e]

toMaybeAbiValue6 :: (AbiValueEncoding a, AbiValueEncoding b, AbiValueEncoding c, AbiValueEncoding d, AbiValueEncoding e, AbiValueEncoding f) => (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e, Maybe f) -> [Maybe AbiValue]
toMaybeAbiValue6 (a, b, c, d, e, f) = [toMaybeAbiValue a, toMaybeAbiValue b, toMaybeAbiValue c, toMaybeAbiValue d, toMaybeAbiValue e, toMaybeAbiValue f]

toMaybeAbiValue7 :: (AbiValueEncoding a, AbiValueEncoding b, AbiValueEncoding c, AbiValueEncoding d, AbiValueEncoding e, AbiValueEncoding f, AbiValueEncoding g) => (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e, Maybe f, Maybe g) -> [Maybe AbiValue]
toMaybeAbiValue7 (a, b, c, d, e, f, g) = [toMaybeAbiValue a, toMaybeAbiValue b, toMaybeAbiValue c, toMaybeAbiValue d, toMaybeAbiValue e, toMaybeAbiValue f, toMaybeAbiValue g]

toMaybeAbiValue8 :: (AbiValueEncoding a, AbiValueEncoding b, AbiValueEncoding c, AbiValueEncoding d, AbiValueEncoding e, AbiValueEncoding f, AbiValueEncoding g, AbiValueEncoding h) => (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e, Maybe f, Maybe g, Maybe h) -> [Maybe AbiValue]
toMaybeAbiValue8 (a, b, c, d, e, f, g, h) = [toMaybeAbiValue a, toMaybeAbiValue b, toMaybeAbiValue c, toMaybeAbiValue d, toMaybeAbiValue e, toMaybeAbiValue f, toMaybeAbiValue g, toMaybeAbiValue h]

toMaybeAbiValue9 :: (AbiValueEncoding a, AbiValueEncoding b, AbiValueEncoding c, AbiValueEncoding d, AbiValueEncoding e, AbiValueEncoding f, AbiValueEncoding g, AbiValueEncoding h, AbiValueEncoding i) => (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e, Maybe f, Maybe g, Maybe h, Maybe i) -> [Maybe AbiValue]
toMaybeAbiValue9 (a, b, c, d, e, f, g, h, i) = [toMaybeAbiValue a, toMaybeAbiValue b, toMaybeAbiValue c, toMaybeAbiValue d, toMaybeAbiValue e, toMaybeAbiValue f, toMaybeAbiValue g, toMaybeAbiValue h, toMaybeAbiValue i]

toMaybeAbiValue10 :: (AbiValueEncoding a, AbiValueEncoding b, AbiValueEncoding c, AbiValueEncoding d, AbiValueEncoding e, AbiValueEncoding f, AbiValueEncoding g, AbiValueEncoding h, AbiValueEncoding i, AbiValueEncoding j) => (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e, Maybe f, Maybe g, Maybe h, Maybe i, Maybe j) -> [Maybe AbiValue]
toMaybeAbiValue10 (a, b, c, d, e, f, g, h, i, j) = [toMaybeAbiValue a, toMaybeAbiValue b, toMaybeAbiValue c, toMaybeAbiValue d, toMaybeAbiValue e, toMaybeAbiValue f, toMaybeAbiValue g, toMaybeAbiValue h, toMaybeAbiValue i, toMaybeAbiValue j]

isDynamicType :: Type -> TypeArray -> Bool
isDynamicType ty tya = case tya of
  DynamicArray -> True
  FixedArray n -> (n > 0) && isDynamicType ty NoArray
  NoArray -> case ty of
    TyBytes -> True
    TyUtf8 -> True
    (TyTuple cs) -> any (uncurry isDynamicType)
                      $ map (abiParamType &&& abiParamTypeArray) cs
    _ -> False

data AbiCallData = AbiCallData
  { abiFunctionSelector :: BS.ByteString
  , abiEncodedAbiValues :: BS.ByteString
  } deriving (Show)

abiCallDataToHexText (AbiCallData fs eav) = bs2hex $ fs <> eav

fromRight (Left e) = error (show e)
fromRight (Right v) = v

type DecodeLogResult = Maybe (Either Text (Text,AbiValue))

maybeDecodeLogResult :: DecodeLogResult -> Maybe (Text,AbiValue)
maybeDecodeLogResult = maybe Nothing (either (const Nothing) Just)

fromDecodeLogResult :: DecodeLogResult -> (Text,AbiValue)
fromDecodeLogResult = fromRight . fromJust

logEventName :: (Text,AbiValue) -> Text
logEventName = fst

logEventValue :: (Text,AbiValue) -> AbiValue
logEventValue = snd

class FromLogEvent a where
  fromLogEvent :: Text -> AbiValue -> Either Text a

data NullEvent = NullEvent deriving (Show)

instance FromLogEvent NullEvent where
  fromLogEvent _ _ = Left "NullEvent"

null_decode_log :: Maybe (RpcEthLog -> Either Text NullEvent)
null_decode_log = Nothing

class ToEventFilter a where
  toEventFilter :: a -> (Text,[Maybe AbiValue])

data CompilationTarget = CompilationTarget
  { metaTargetPath :: FilePath
  , metaTargetName :: Text
  } deriving (Eq, Show)

instance FromJSON CompilationTarget where
  parseJSON (Object o) = do
    let (fp,String n) = head (LHM.toList o)
    return $ CompilationTarget (T.unpack fp) n

data Optimizer = Optimizer
  { metaOptimizerRuns :: Int
  , metaOptimizerEnabled :: Bool
  } deriving (Eq, Show)

instance FromJSON Optimizer where
  parseJSON (Object o) = Optimizer
                     <$> o .: "runs"
                     <*> o .: "enabled"

data Settings = Settings
  { metaSettingsCompilationTarget :: CompilationTarget
  , metaSettingsRemappings :: [Text]
  , metaSettingsOptimizer :: Optimizer
  , metaSettingsLibraries :: Value
  } deriving (Eq, Show)

instance FromJSON Settings where
  parseJSON (Object o) = Settings
                     <$> o .: "compilationTarget"
                     <*> o .: "remappings"
                     <*> o .: "optimizer"
                     <*> o .: "libraries"

data Source = Source
  { metaSourcePath :: FilePath
  , metaSourceBzzr :: HexHash256
  , metaSourceHash :: HexHash256
  } deriving (Eq, Show)

parseSource fp (Object o) = Source (T.unpack fp)
                        <$> (joinHex . T.drop 7 . head <$> o .: "urls")
                        <*> o .: "keccak256"

data Output = Output
  { metaOutputDevdoc :: Value
  , metaOutputAbi :: [Interface]
  , metaOutputUserdoc :: Value
  } deriving (Eq, Show)

instance FromJSON Output where
  parseJSON (Object o) = Output
                     <$> o .: "devdoc"
                     <*> o .: "abi"
                     <*> o .: "userdoc"

data ProgLanguage = Solidity deriving (Eq, Show)

instance FromJSON ProgLanguage where
  parseJSON (String "Solidity") = return Solidity

newtype Compiler = Compiler { metaCompilerVersion :: Text } deriving (Eq, Show)

instance FromJSON Compiler where
  parseJSON (Object o) = Compiler <$> o .: "version"

data Metadata = Metadata
  { metaSettings :: Settings
  , metaSources :: [Source]
  , metaOutput :: Output
  , metaVersion :: Int
  , metaLanguage :: ProgLanguage
  , metaCompiler :: Compiler
  } deriving (Eq, Show)

instance FromJSON Metadata where
  parseJSON (Object o) = Metadata
                     <$> o .: "settings"
                     <*> parseListOfKeyObject o "sources" parseSource
                     <*> o .: "output"
                     <*> o .: "version"
                     <*> o .: "language"
                     <*> o .: "compiler"

