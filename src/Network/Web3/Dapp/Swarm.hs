{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

module Network.Web3.Dapp.Swarm
  ( SwarmSettings(..)
  , defaultSwarmSettings
  , swarm_up_bzzr
  , swarm_stdin_up_bzzr
  , swarm_get_bzzr
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Network.Curl
import Network.Curl.Code (CurlCode(..))
import Network.Web3.HexText
import Network.Web3.Types
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

-- | Opciones para /swarm/
data SwarmSettings = SwarmSettings
  { swarmBzzApiHost :: String
  , swarmBzzApiPort :: Int
  } deriving (Show)

-- | Opciones por defecto para /swarm/: __http:\/\/localhost:8500__
defaultSwarmSettings :: SwarmSettings
defaultSwarmSettings = SwarmSettings "localhost" 8500

swarmBzzApi :: SwarmSettings -> String
swarmBzzApi (SwarmSettings bzzApiHost bzzApiPort) =
  "http://" ++ bzzApiHost ++ ":" ++ show bzzApiPort

swarm :: SwarmSettings -> [String] -> Maybe BS.ByteString
      -> IO (Either Text HexHash256)
swarm ops args mStdIn = do
  mSwarmFp <- findExecutable "swarm"
  case mSwarmFp of
    Nothing -> return $ Left "swarm executable not found."
    Just swarmFp -> do
      (ec, out, err) <- readProcessWithExitCode swarmFp
                          ("--bzzapi":swarmBzzApi ops:args)
                          (maybe "" C8.unpack mStdIn)
      return $ case ec of
        ExitFailure c -> Left $ "swarm failed with exit code "
                             <> T.pack (show c) <> ": "
                             <> T.pack out <> " "
                             <> T.pack err
        ExitSuccess -> Right $ joinHex $ T.pack $ init out

-- | Sube un fichero a /swarm/ y devuelve el hash bzzr:/
swarm_up_bzzr :: SwarmSettings -> FilePath -> IO (Either Text HexHash256)
swarm_up_bzzr ops fp = swarm ops ["--manifest=false", "up", fp] Nothing

-- | Sube contenido binario a /swarm/ y devuelve el hash bzzr:/
swarm_stdin_up_bzzr :: SwarmSettings -> BS.ByteString
                    -> IO (Either Text HexHash256)
swarm_stdin_up_bzzr ops = swarm ops ["--manifest=false", "--stdin", "up"] . Just

type SwarmResponse = CurlResponse_ [(String,String)] LBS.ByteString

-- | Descarga el contenido asociado al hash bzzr:/ de /swarm/
swarm_get_bzzr :: SwarmSettings -> HexHash256 -> IO (Either Text BS.ByteString)
swarm_get_bzzr ops bzzr = do
  let url = swarmBzzApi ops ++ "/bzzr:/" ++ T.unpack (stripHex bzzr)
  cResp <- (curlGetResponse_ url [] :: IO SwarmResponse)
  let respCode = respCurlCode cResp
  return $ case respCode of
    CurlOK -> Right $ LBS.toStrict $ respBody cResp
    _ -> Left $ "curlGet: GET error: " <> T.pack (show respCode)

