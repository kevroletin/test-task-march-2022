{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TrialChain.Client
  ( addTx,
    getTx,
    getBalance,
  )
where

import Control.DeepSeq
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Protolude
import Servant.API
import Servant.Client
import TrialChain.API
import TrialChain.Signature
import TrialChain.Types

addTx :: Tx -> ClientM ()
getTx :: Hash -> ClientM Tx
getBalance :: PublicKey -> ClientM Money
addTx :<|> getTx :<|> getBalance = client trialChainAPI

instance ToHttpApiData Hash where
  toUrlPiece = unHash

instance ToHttpApiData PublicKey where
  toUrlPiece = unPublicKey
