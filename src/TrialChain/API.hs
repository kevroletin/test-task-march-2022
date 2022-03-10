{-# LANGUAGE TypeOperators #-}

module TrialChain.API where

import Servant
import TrialChain.Types

{- ORMOLU_DISABLE -}
type TrialChainAPI =
         "tx" :> ReqBody '[JSON] Tx :> Put '[JSON] ()
    :<|> "tx" :> Capture "txId" Hash :> Get '[JSON] Tx
    :<|> "balance" :> Capture "publicKey" PublicKey :> Get '[JSON] Money
{- ORMOLU_ENABLE -}

trialChainAPI :: Proxy TrialChainAPI
trialChainAPI = Proxy
