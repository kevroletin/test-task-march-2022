module TrialChain.Client
  ( addTx,
    getTx,
    getBalance,
    alive,
  )
where

import Protolude
import Servant.API ((:<|>) (..))
import Servant.Client (ClientM, client)
import TrialChain.API (trialChainAPI)
import TrialChain.Types (Hash (..), Money, PublicKey (..), Tx)

addTx :: Tx -> ClientM ()
getTx :: Hash -> ClientM Tx
getBalance :: PublicKey -> ClientM Money
alive :: ClientM Bool
addTx :<|> getTx :<|> getBalance :<|> alive = client trialChainAPI
