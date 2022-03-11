{-# OPTIONS_GHC -fno-warn-orphans #-}

module TrialChain.Server
  ( ServerState (..),
    trialChainServer,
    trialChainApp,
  )
where

import Control.Concurrent.STM (TVar, readTVar, writeTVar)
import qualified Data.ByteString.Lazy as BL
import Protolude hiding (Handler)
import Servant (Application, FromHttpApiData (..), Handler, Server, err400, errBody, serve, (:<|>) (..))
import TrialChain.API (TrialChainAPI, trialChainAPI)
import TrialChain.Simulator (SimM, SimState, runSimM)
import qualified TrialChain.Simulator as Sim
import TrialChain.Types (AppError, Hash (..), Money (..), PublicKey (..), Tx (..), mkHash)

instance FromHttpApiData Hash where
  parseUrlPiece = mkHash

instance FromHttpApiData PublicKey where
  parseUrlPiece = second PublicKey . parseUrlPiece

commitSimM :: ServerState -> SimM a -> Handler (Either AppError a)
commitSimM (ServerState stT) act = liftIO . atomically $ do
  st <- readTVar stT
  case runSimM act st of
    Left err -> pure (Left err)
    Right (res, newSt) -> do
      writeTVar stT newSt
      pure (Right res)

returnAppRes :: Either AppError a -> Handler a
returnAppRes (Left err) = throwError $ err400 {errBody = (BL.fromStrict . encodeUtf8 . show) err}
returnAppRes (Right x) = return x

appM2handler :: ServerState -> SimM a -> Handler a
appM2handler ss act = commitSimM ss act >>= returnAppRes

addTxH :: ServerState -> Tx -> Handler ()
addTxH ss tx =
  appM2handler ss $ Sim.addTx tx

getTxH :: ServerState -> Hash -> Handler Tx
getTxH ss txId =
  appM2handler ss (Sim.getTx txId)

getBalanceH :: ServerState -> PublicKey -> Handler Money
getBalanceH ss pubKey =
  appM2handler ss (Sim.getBalance pubKey)

newtype ServerState = ServerState (TVar SimState)

trialChainServer :: ServerState -> Server TrialChainAPI
trialChainServer st =
  addTxH st
    :<|> getTxH st
    :<|> getBalanceH st

trialChainApp :: ServerState -> Application
trialChainApp ss = serve trialChainAPI (trialChainServer ss)
