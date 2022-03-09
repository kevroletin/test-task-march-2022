module TrialChain.AppState
  ( AppError (..),
    mkState,
    getBalance,
    addTx,
    getTx,
  )
where

import qualified Data.Map.Strict as Map
import Protolude
import TrialChain.Signature
import TrialChain.Types

data AppState = AppState
  { app_transactions :: Map Hash Tx,
    app_balances :: Map PublicKey Money
  }
  deriving (Show, Eq)

data AppError
  = InvalidTxSignature Signature
  | InsufficientFunds {err_balance :: Money, err_amount :: Money}
  | DuplicateTx Hash
  | UnknownTx Hash
  deriving (Eq, Show, Generic)

mkState :: [(PublicKey, Integer)] -> AppState
mkState list = AppState mempty balances
  where
    balances = Map.fromListWith (\(Money a) (Money b) -> Money (a + b)) moneyList
    moneyList = fmap (\(a, b) -> (a, Money b)) list

getBalance :: AppState -> PublicKey -> Money
getBalance AppState {..} pubKey =
  fromMaybe (Money 0) $ Map.lookup pubKey app_balances

addTx :: AppState -> Tx -> Either AppError AppState
addTx st@AppState {..} tx@(Tx txb@TxBody {..} _) = do
  let (Money balanceFrom) = getBalance st txb_from
      (Money balanceTo) = getBalance st txb_to
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
    AppState
      { app_transactions = Map.insert txId tx app_transactions,
        app_balances = newBalances
      }

getTx :: AppState -> Hash -> Either AppError Tx
getTx AppState {..} txId =
  maybeToRight (UnknownTx txId) $ Map.lookup txId app_transactions
