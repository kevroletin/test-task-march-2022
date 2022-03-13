-- | Simulator of TrialChain blockcain
--
-- * it validates tx signature;
--
-- * refuses double spend;
--
-- * refuses sending non existing money;
--
-- It also tracks some state:
--
-- * commited txs;
--
-- * account balances.
module TrialChain.Simulator.Pure
  ( SimState (..),
    mkState,
    getBalance,
    addTx,
    getTx,
  )
where

import qualified Data.Map.Strict as Map
import Numeric.Natural (Natural)
import Protolude
import TrialChain.Signature (hashTxBody, validateTxSign)
import TrialChain.Types (AppError (..), Hash (..), Money (..), PublicKey (..), Tx (..), TxBody (..))

data SimState = SimState
  { app_transactions :: Map Hash Tx,
    app_balances :: Map PublicKey Money
  }
  deriving (Show, Eq)

mkState :: [(PublicKey, Natural)] -> SimState
mkState xs = SimState mempty balances
  where
    balances = Map.fromListWith (\a b -> Money (unMoney a + unMoney b)) moneyList
    moneyList = fmap (second Money) xs

getBalance :: PublicKey -> SimState -> Money
getBalance pubKey SimState {..} =
  fromMaybe (Money 0) $ Map.lookup pubKey app_balances

addTx :: Tx -> SimState -> Either AppError SimState
addTx tx@(Tx txb@TxBody {..} _) st@SimState {..} = do
  let balanceFrom = unMoney $ getBalance txb_from st
      balanceTo = unMoney $ getBalance txb_to st
      amount = unMoney $ txb_amount
      -- Num instance of Natural sucks because it throws
      newBalanceFrom =
        if balanceFrom < amount then Money 0 else Money (balanceFrom - amount)
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
  when (balanceFrom < amount) $
    throwError (InsufficientFunds (Money balanceFrom) txb_amount)
  pure $
    SimState
      { app_transactions = Map.insert txId tx app_transactions,
        app_balances = newBalances
      }

getTx :: Hash -> SimState -> Either AppError Tx
getTx txId SimState {..} =
  maybeToRight (UnknownTx txId) $ Map.lookup txId app_transactions
