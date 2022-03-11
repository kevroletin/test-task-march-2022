{-# OPTIONS_GHC -fno-warn-orphans #-}

module TrialChain.Client
  ( addTx,
    getTx,
    getBalance,
  )
where

import Servant.API (ToHttpApiData, toUrlPiece, (:<|>) (..))
import Servant.Client (ClientM, client)
import TrialChain.API (trialChainAPI)
import TrialChain.Types (Hash (..), Money, PublicKey (..), Tx)

addTx :: Tx -> ClientM ()
getTx :: Hash -> ClientM Tx
getBalance :: PublicKey -> ClientM Money
addTx :<|> getTx :<|> getBalance = client trialChainAPI

instance ToHttpApiData Hash where
  toUrlPiece = unHash

instance ToHttpApiData PublicKey where
  toUrlPiece = unPublicKey
