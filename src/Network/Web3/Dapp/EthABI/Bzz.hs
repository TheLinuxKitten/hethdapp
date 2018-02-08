{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

module Network.Web3.Dapp.EthABI.Bzz
  ( getBinBzzr0
  , uploadMetadata
  , downloadMetadata
  ) where

import Control.Arrow ((&&&),(***))
import Data.Aeson
import Data.Aeson.JsonUtils
import Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List.Split (splitOn)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Network.Web3.Dapp.EthABI (decodeMetadata, lenUint256)
import Network.Web3.Dapp.EthABI.Types
import Network.Web3.Dapp.Swarm
import Network.Web3.HexText
import Network.Web3.Types

-- | Obtiene el posible hash bzzr0 contenido en código binario de un contract.
getBinBzzr0 :: HexData -> Maybe HexHash256
getBinBzzr0 hdat =
  let (_:r1) = splitOn "a165627a7a72305820" (T.unpack hdat)
  in if null r1 then Nothing
      else let lenHash = lenUint256 * 2
               (mhash,r2) = splitAt lenHash (concat r1)
               sufix = take 4 r2
           in if length mhash == lenHash && sufix == "0029"
                then Just (joinHex $ T.pack mhash)
                else Nothing

-- | Sube a /swarm/ el metadata de un contract dado su código binario. También
-- sube el contenido de los fuentes usados en la compilación (ver `Metadata`).
-- Comprueba que los hashes devueltos por /swarm/ son los esperados.
--
-- Es más seguro y recomendable usar la función generada por el módulo
-- "Network.Web3.Dapp.EthABI.TH"
uploadMetadata :: SwarmSettings -> HexData -> LBS.ByteString
               -> IO (Either Text (HexHash256,[(FilePath,HexHash256)]))
uploadMetadata ops binCode metaBs = do
  let eSrcBzzs = map (metaSourcePath &&& metaSourceBzzr) . metaSources
             <$> decodeMetadata metaBs
  case eSrcBzzs of
    Left e1 -> return $ Left e1
    Right srcBzzs -> do
      let mBzz0 = getBinBzzr0 binCode
      case mBzz0 of
        Nothing -> return
                 $ Left "uploadMetadata: binary code doesn't contain swarm url"
        Just bzz0 -> do
          eBzzs <- sequence
               <$> mapM (swarm_up_bzzr ops . fst) srcBzzs
          eBzz0 <- swarm_stdin_up_bzzr ops (LBS.toStrict metaBs)
          return (eBzz0
              >>= \bzz0' -> eBzzs
              >>= \bzzs -> chkBzzs (bzz0:map snd srcBzzs) (bzz0':bzzs)
               >> Right (bzz0,srcBzzs))
  where
    chkBzzs bzzs1 bzzs2 =
      if any (not . uncurry (==)) (zip bzzs1 bzzs2)
        then Left $ "chkBzzs: hashes don't match: "
                 <> T.pack (show bzzs1) <> " "
                 <> T.pack (show bzzs2)
        else Right ()

-- | Descarga de /swarm/ la información asociada a un contract, si existe.
--
-- Es más seguro y recomendable usar la función `Network.Web3.Dapp.EthABI.TH.downloadHttp` o `Network.Web3.Dapp.EthABI.TH.downloadIpc`
-- del módulo "Network.Web3.Dapp.EthABI.TH"
downloadMetadata :: SwarmSettings -> HexData -> IO (Either Text (Contract,[(FilePath,Either Text BS.ByteString)]))
downloadMetadata ops binCode = do
  -- get code -> bzzr0
  let mBzzr0 = getBinBzzr0 binCode
  case mBzzr0 of
    Nothing -> return $ Left "downloadMetadata: code doesn't contain bzzr0 url"
    Just bzzr0 -> do
      eMetaBs <- fmap LBS.fromStrict
             <$> swarm_get_bzzr defaultSwarmSettings bzzr0
      case eMetaBs of
        Left e1 -> return (Left e1)
        Right metaBs -> do
          let eMeta = decodeMetadata metaBs
          let eSrcs = map metaSourcePath . metaSources <$> eMeta
          let eBzzs = map metaSourceBzzr . metaSources <$> eMeta
          case eBzzs of
            Left e2 -> return (Left e2)
            Right bzzs -> do
              eBss <- mapM (swarm_get_bzzr ops) bzzs
              let srcs = fromRight eSrcs
              let meta = fromRight eMeta
              let nom = metaTargetName $ metaSettingsCompilationTarget
                                       $ metaSettings meta
              let fp = metaTargetPath $ metaSettingsCompilationTarget
                                      $ metaSettings meta
              let abi = metaOutputAbi $ metaOutput meta
              let contract = Contract nom fp abi Nothing binCode metaBs
              return $ Right (contract, zip srcs eBss)

