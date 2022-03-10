{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Concurrent.STM
import Control.Monad.Except
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding.Base16 as Base16
import Network.Wai
import Network.Wai.Handler.Warp
import Protolude hiding (Handler)
import Servant
import TrialChain.AppState
import TrialChain.Types

{- ORMOLU_DISABLE -}
type API =
         "tx" :> ReqBody '[JSON] Tx :> Put '[JSON] ()
    :<|> "tx" :> Capture "txId" Hash :> Get '[JSON] Tx
    :<|> "balance" :> Capture "publicKey" PublicKey :> Get '[JSON] Money
{- ORMOLU_ENABLE -}

instance ToJSON Tx

instance FromJSON Tx

instance ToJSON TxBody

instance FromJSON TxBody

instance ToJSON Money

instance FromJSON Money

instance ToJSON PublicKey

instance FromJSON PublicKey

instance ToJSON Signature

instance FromJSON Signature

instance FromHttpApiData Hash where
  parseUrlPiece = mkHash

instance FromHttpApiData PublicKey where
  parseUrlPiece = second PublicKey . parseUrlPiece

commitAppM :: ServerState -> AppM a -> Handler (Either AppError a)
commitAppM (ServerState stT) act = liftIO . atomically $ do
  st <- readTVar stT
  case runAppM act st of
    Left err -> pure (Left err)
    Right (res, newSt) -> do
      writeTVar stT newSt
      pure (Right res)

returnAppRes :: Either AppError a -> Handler a
returnAppRes (Left err) = throwError $ err400 {errBody = (BL.fromStrict . encodeUtf8 . show) err}
returnAppRes (Right x) = return x

appM2handler :: ServerState -> AppM a -> Handler a
appM2handler ss act = commitAppM ss act >>= returnAppRes

addTxH :: ServerState -> Tx -> Handler ()
addTxH ss tx =
  appM2handler ss $ addTxM tx

getTxH :: ServerState -> Hash -> Handler Tx
getTxH ss txId =
  appM2handler ss (getTxM txId)

getBalanceH :: ServerState -> PublicKey -> Handler Money
getBalanceH ss pubKey =
  appM2handler ss (getBalanceM pubKey)

newtype ServerState = ServerState (TVar AppState)

server :: ServerState -> Server API
server st =
  addTxH st
    :<|> getTxH st
    :<|> getBalanceH st

api :: Proxy API
api = Proxy

app :: ServerState -> Application
app ss = serve api (server ss)

main :: IO ()
main = do
  storage <- newTVarIO $ mkState [(PublicKey "bank", 99999999999999)]
  run 8081 (app (ServerState storage))
