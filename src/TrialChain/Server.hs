{-# OPTIONS_GHC -fno-warn-orphans #-}

module TrialChain.Server
  ( ServerState (..),
    trialChainServer,
    trialChainApp,
  )
where

import Control.Concurrent.STM
import Control.Monad.Except
import qualified Data.ByteString.Lazy as BL
import Network.Wai
import Protolude hiding (Handler)
import Servant
import TrialChain.API
import TrialChain.AppState
import TrialChain.Types

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

trialChainServer :: ServerState -> Server TrialChainAPI
trialChainServer st =
  addTxH st
    :<|> getTxH st
    :<|> getBalanceH st

trialChainApp :: ServerState -> Application
trialChainApp ss = serve trialChainAPI (trialChainServer ss)
