{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

mkTx :: PrivateKey -> PublicKey -> Integer -> Text -> Tx
mkTx privFrom pubTo amount nonce =
  signTxBody privFrom $
    TxBody
      { txb_from = priv2pub privFrom,
        txb_to = pubTo,
        txb_amount = Money amount,
        txb_nonce = nonce
      }

mkTx' :: PrivateKey -> PublicKey -> Integer -> Text -> (Hash, Tx)
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
  let txs = [mkTx' privBank pubKey 1 (show i) | i <- [1 .. 10_000]]
  putStrLn ("-> generating transactions" :: Text)
  _ <- deepSeqM txs
  putStrLn ("-> adding txs" :: Text)
  forM_ (txs `zip` [1 ..]) $ \((_, tx), i) -> do
    putStr (show i <> "\r" :: Text)
    addTx tx
  putStrLn ("-> requesting txs" :: Text)
  forM_ (txs `zip` [1 ..]) $ \((txId, _), i) -> do
    putStr (show i <> "\r" :: Text)
    getTx txId
  getBalance pubKey

main :: IO ()
main = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM test (mkClientEnv manager' (BaseUrl Http "localhost" 8081 ""))
  print res
