{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeOperators #-}

import Network.HTTP.Client (defaultManagerSettings, newManager)
import Protolude
import Servant.Client (BaseUrl (..), ClientM, Scheme (..), mkClientEnv, runClientM)
import TrialChain.Client (addTx, getBalance, getTx)
import TrialChain.Signature (hashTxBody, mkAccount, mkTx)
import TrialChain.Types (Hash (..), Money (..), Natural, PrivateKey (..), PublicKey (..), Tx (..))

mkTx' :: PrivateKey -> PublicKey -> Natural -> Text -> (Hash, Tx)
mkTx' fromPriv toPub amount nonce =
  let tx = mkTx fromPriv toPub amount nonce in (hashTxBody (tx_body tx), tx)

deepSeqM :: (Monad m, NFData a) => a -> m a
deepSeqM x = do
  () <- pure (x `deepseq` ())
  return x

-- 1. generate a bunch of transactions from the special account to the same
--    account.
-- 2. commit txs.
-- 3. get each tx
-- 4. get final balance
test :: ClientM Money
test = do
  let (_, privBank) = mkAccount "bank"
  let (pubKey, _) = mkAccount "basilio"
  let txs = [mkTx' privBank pubKey 1 (show i) | i <- [1 .. (10_000 :: Int)]]
  putStrLn ("-> generating transactions" :: Text)
  _ <- deepSeqM txs
  putStrLn ("-> adding txs" :: Text)
  forM_ (txs `zip` [(1 :: Int) ..]) $ \((_, tx), i) -> do
    putStr (show i <> "\r" :: Text)
    addTx tx
  putStrLn ("-> requesting txs" :: Text)
  forM_ (txs `zip` [(1 :: Int) ..]) $ \((txId, _), i) -> do
    putStr (show i <> "\r" :: Text)
    getTx txId
  getBalance pubKey

main :: IO ()
main = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM test (mkClientEnv manager' (BaseUrl Http "localhost" 8081 ""))
  print res
