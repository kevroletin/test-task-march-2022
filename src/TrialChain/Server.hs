module TrialChain.Server
  ( ServerState (..),
    trialChainServer,
    trialChainApp,
  )
where

import Control.Concurrent.STM (TVar, readTVar, writeTVar)
import qualified Data.ByteString.Lazy as BL
import Protolude hiding (Handler)
import Servant (Application, Handler, Server, err400, errBody, serve, (:<|>) (..))
import TrialChain.API (TrialChainAPI, trialChainAPI)
import TrialChain.Simulator (SimM, SimState, runSimM)
import qualified TrialChain.Simulator as Sim
import TrialChain.Types (AppError, Hash (..), Money (..), PublicKey (..), Tx (..))

newtype ServerState = ServerState (TVar SimState)

commitSimM :: ServerState -> SimM a -> Handler (Either AppError a)
commitSimM (ServerState stT) act = liftIO . atomically $ do
  st <- readTVar stT
  case runSimM act st of
    Left err -> pure (Left err)
    Right (res, newSt) -> do
      writeTVar stT newSt
      pure (Right res)

returnAppRes :: Either AppError a -> Handler a
returnAppRes (Left err) =
  throwError $ err400 {errBody = (BL.fromStrict . encodeUtf8 . show) err}
returnAppRes (Right x) =
  return x

simM2handler :: ServerState -> SimM a -> Handler a
simM2handler st act = commitSimM st act >>= returnAppRes

addTxH :: ServerState -> Tx -> Handler ()
addTxH st tx =
  simM2handler st $ Sim.addTx tx

getTxH :: ServerState -> Hash -> Handler Tx
getTxH st txId =
  simM2handler st (Sim.getTx txId)

getBalanceH :: ServerState -> PublicKey -> Handler Money
getBalanceH st pubKey =
  simM2handler st (Sim.getBalance pubKey)

trialChainServer :: ServerState -> Server TrialChainAPI
trialChainServer st =
  addTxH st
    :<|> getTxH st
    :<|> getBalanceH st
    :<|> pure True

trialChainApp :: ServerState -> Application
trialChainApp st = serve trialChainAPI (trialChainServer st)
