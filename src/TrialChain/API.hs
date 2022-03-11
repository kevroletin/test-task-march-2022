module TrialChain.API
  ( trialChainAPI,
    TrialChainAPI,
  )
where

import Protolude
import Servant (Capture, Get, JSON, Proxy (..), Put, ReqBody, (:<|>), (:>))
import TrialChain.Types (AppError, Hash, Money, PublicKey, Tx)

{- ORMOLU_DISABLE -}
type TrialChainAPI =
         "tx" :> ReqBody '[JSON] Tx :> Put '[JSON] ()
    :<|> "tx" :> Capture "txId" Hash :> Get '[JSON] Tx
    :<|> "balance" :> Capture "publicKey" PublicKey :> Get '[JSON] Money
    :<|> "alive" :> Get '[JSON] Bool
{- ORMOLU_ENABLE -}

trialChainAPI :: Proxy TrialChainAPI
trialChainAPI = Proxy
