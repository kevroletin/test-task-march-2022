module TrialChain.AppState
  ( AppError (..),
    mkState,
    getBalance,
    addTx,
    getTx,
    evalAppM,
    getBalanceM,
    addTxM,
    getTxM,
  )
where

import Control.Monad.Except (liftEither)
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
    moneyList = fmap (second Money) list

getBalance :: PublicKey -> AppState -> Money
getBalance pubKey AppState {..} =
  fromMaybe (Money 0) $ Map.lookup pubKey app_balances

addTx :: Tx -> AppState -> Either AppError AppState
addTx tx@(Tx txb@TxBody {..} _) st@AppState {..} = do
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
    AppState
      { app_transactions = Map.insert txId tx app_transactions,
        app_balances = newBalances
      }

getTx :: Hash -> AppState -> Either AppError Tx
getTx txId AppState {..} =
  maybeToRight (UnknownTx txId) $ Map.lookup txId app_transactions

newtype AppM a = App {unApp :: StateT AppState (ExceptT AppError Identity) a}
  deriving newtype (Functor, Applicative, Monad, MonadState AppState, MonadError AppError)

evalAppM :: AppM a -> AppState -> Either AppError a
evalAppM appM st = runExcept (evalStateT (unApp appM) st)

getBalanceM :: PublicKey -> AppM Money
getBalanceM pubKey = get <&> getBalance pubKey

addTxM :: Tx -> AppM ()
addTxM tx = get >>= liftEither . addTx tx >>= put

getTxM :: Hash -> AppM Tx
getTxM txId = get >>= (liftEither . getTx txId)
