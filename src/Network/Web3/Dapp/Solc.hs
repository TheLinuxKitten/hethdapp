{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

module Network.Web3.Dapp.Solc
  ( SolcSettings(..)
  , SolcRemapping(..)
  , compile
  , fst'
  , snd'
  , trd'
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Network.Web3.Dapp.EthABI
import Network.Web3.Dapp.EthABI.AST
import Network.Web3.Dapp.EthABI.Types

data SolcRemapping = SolcRemapping
  { remapContext :: Maybe String
  , remapPrefix :: String
  , remapTarget :: Maybe FilePath
  } deriving (Show)

solcRemappingToArg :: SolcRemapping -> String
solcRemappingToArg (SolcRemapping mctxt prefix mtarget) =
  let ctxt = maybe "" (++":") mctxt
      target = maybe "" ("="++) mtarget
  in ctxt ++ prefix ++ target

fst' :: (a,b,c) -> a
fst' (a,_,_) = a

snd' :: (a,b,c) -> b
snd' (_,b,_) = b

trd' :: (a,b,c) -> c
trd' (_,_,c) = c

-- | Settings del compilador solc.
data SolcSettings = SolcSettings
  { solcRemappings :: [SolcRemapping]   -- ^ Lista de remappings
  , solcExcludes :: [Text]    -- ^ Lista de nombres de contracts a excluir de los resultados.
  } deriving (Show)

-- | Compila fuentes __Solidity__ con el compilador /solc/. Devuelve tres datos: La lista de contracts, la lista de ASTs y toda la salida JSON del compilador.
compile :: SolcSettings   -- ^ Settings del compilador
        -> [FilePath]     -- ^ Lista de ficheros fuente __Solidity__
        -> IO (Either Text [Contract], Either Text [SolcAST], Either Text Value)
compile stgs fps = do
  mSolcFp <- findExecutable "solc"
  case mSolcFp of
    Nothing -> do
      let err1 = Left "compile: solc executable not found."
      return (err1,err1,err1)
    Just solcFp -> do
      let solcArgs = [ "--optimize"
                     , "--combined-json", "ast,abi,bin,bin-runtime,interface,metadata"
                     ]
      let remappings = map solcRemappingToArg $ solcRemappings stgs
      let args = solcArgs ++ remappings ++ fps
      (ec, out, err) <- readProcessWithExitCode solcFp args ""
      case ec of
        ExitFailure c -> do
          let err2 = Left $ "compile: solc failed with exit code "
                         <> T.pack (show c) <> ": "
                         <> T.pack out <> " "
                         <> T.pack err
          return (err2,err2,err2)
        ExitSuccess -> do
          let bs = LC8.pack out
          let ecs = filter (not . isExcludedContract) <$> decodeSolcAbi bs
    --          easts = ecs >>= \cs -> filter (isContractAST cs) <$> decodeSolcAst bs
              eval = case eitherDecode bs of
                        Left e -> Left $ T.pack e
                        Right v -> Right v
          return (ecs, decodeSolcAst bs, eval)
  where
    isExcludedContract = (`elem` (solcExcludes stgs)) . abiContractName
--    isContractAST cs = (`elem` (map abiContractPath cs)) . astSource

