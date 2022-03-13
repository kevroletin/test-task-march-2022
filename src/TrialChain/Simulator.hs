-- | Monadic interface to TrialChain.Simulator.Pure
module TrialChain.Simulator
  ( SimState (..),
    SimM (..),
    Super.mkState,
    evalSimM,
    runSimM,
    getBalance,
    addTx,
    getTx,
  )
where

import Control.Monad.Except (liftEither)
import Protolude
import TrialChain.Simulator.Pure (SimState (..))
import qualified TrialChain.Simulator.Pure as Super
import TrialChain.Types (AppError (..), Hash (..), Money (..), PublicKey (..), Tx (..))

newtype SimM a = Sim {unApp :: StateT SimState (ExceptT AppError Identity) a}
  deriving newtype (Functor, Applicative, Monad, MonadState SimState, MonadError AppError)

evalSimM :: SimM a -> SimState -> Either AppError a
evalSimM appM st = runExcept (evalStateT (unApp appM) st)

runSimM :: SimM a -> SimState -> Either AppError (a, SimState)
runSimM appM st = runExcept (runStateT (unApp appM) st)

getBalance :: PublicKey -> SimM Money
getBalance pubKey = get <&> Super.getBalance pubKey

addTx :: Tx -> SimM ()
addTx tx = get >>= liftEither . Super.addTx tx >>= put

getTx :: Hash -> SimM Tx
getTx txId = get >>= (liftEither . Super.getTx txId)
