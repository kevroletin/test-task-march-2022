module TrialChain.Simulator.Pure
  ( SimState (..),
    mkState,
    getBalance,
    addTx,
    getTx,
  )
where

import qualified Data.Map.Strict as Map
import Protolude
import TrialChain.Signature (hashTxBody, validateTxSign)
import TrialChain.Types (AppError (..), Hash (..), Money (..), PublicKey (..), Signature (..), Tx (..), TxBody (..))

data SimState = SimState
  { app_transactions :: Map Hash Tx,
    app_balances :: Map PublicKey Money
  }
  deriving (Show, Eq)

mkState :: [(PublicKey, Integer)] -> SimState
mkState xs = SimState mempty balances
  where
    balances = Map.fromListWith (\(Money a) (Money b) -> Money (a + b)) moneyList
    moneyList = fmap (second Money) xs

getBalance :: PublicKey -> SimState -> Money
getBalance pubKey SimState {..} =
  fromMaybe (Money 0) $ Map.lookup pubKey app_balances

addTx :: Tx -> SimState -> Either AppError SimState
addTx tx@(Tx txb@TxBody {..} _) st@SimState {..} = do
  let (Money balanceFrom) = getBalance txb_from st
      (Money balanceTo) = getBalance txb_to st
      (Money amount) = txb_amount
      newBalanceFrom = Money (balanceFrom - amount)
      newBalanceTo = Money (balanceTo + amount)
      txId = hashTxBody txb
      newBalances =
        app_balances
          & Map.insert txb_from newBalanceFrom
          & Map.insert txb_to newBalanceTo

  when (isNothing (validateTxSign tx)) $
    throwError (InvalidTxSignature (tx_signature tx))
  when (isJust (Map.lookup txId app_transactions)) $
    throwError (DuplicateTx txId)
  when (newBalanceFrom < Money 0) $
    throwError (InsufficientFunds (Money balanceFrom) txb_amount)
  pure $
    SimState
      { app_transactions = Map.insert txId tx app_transactions,
        app_balances = newBalances
      }

getTx :: Hash -> SimState -> Either AppError Tx
getTx txId SimState {..} =
  maybeToRight (UnknownTx txId) $ Map.lookup txId app_transactions
