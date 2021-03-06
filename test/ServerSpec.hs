module ServerSpec
  ( serverSpec1,
    serverSpec2,
  )
where

import Control.Concurrent.STM (newTVarIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Search as BS
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types (methodGet, methodPut, statusCode)
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Test (SResponse)
import Protolude hiding (get, put)
import Servant.Client (ClientError (..), ResponseF (..), baseUrlPort, mkClientEnv, parseBaseUrl, runClientM)
import Test.Hspec (Expectation, Spec, around, describe, expectationFailure, it, runIO, shouldBe)
import Test.Hspec.Wai (WaiSession, request, shouldRespondWith, with)
import qualified TrialChain.Client as Client
import TrialChain.Server (ServerState (..), trialChainApp)
import TrialChain.Signature (hashTx, mkAccount, mkTx, signTxBody)
import TrialChain.Simulator (mkState)
import TrialChain.Types (Money (..), PublicKey (..), TxBody (..))

initApp :: IO Application
initApp = do
  storage <- newTVarIO $ mkState [(PublicKey "bank", 99999999999999)]
  pure $ trialChainApp (ServerState storage)

headers :: (IsString a, IsString b) => [(a, b)]
headers = [("Content-type", "application/json")]

get :: ByteString -> WaiSession st SResponse
get path = request methodGet path headers ""

put :: ByteString -> BL.ByteString -> WaiSession st SResponse
put path = request methodPut path headers

withApp :: (Warp.Port -> IO ()) -> IO ()
withApp = Warp.testWithApplication initApp

isSubstring :: ByteString -> ByteString -> Bool
isSubstring small big =
  if BS.null small
    then True
    else not (BS.null x)
  where
    (_, x) = BS.breakOn small big

errorContains :: HasCallStack => Either ClientError a -> ByteString -> Expectation
errorContains (Right _) _ =
  expectationFailure "Expecting error, but scenario finished successfully"
errorContains (Left (FailureResponse _ Response {..})) expBodySubstring = do
  (statusCode responseStatusCode) `shouldBe` 400
  unless (isSubstring expBodySubstring (BL.toStrict responseBody)) $ do
    expectationFailure ("Expecting " <> show expBodySubstring <> " to be part of error message. Got " <> show responseBody)
errorContains (Left resp) _ =
  expectationFailure ("Expecting FailureResponse but got " <> show resp)

-- | Test using type-safe TrialChain.Client
serverSpec1 :: Spec
serverSpec1 = do
  let (pubBank, privBank) = mkAccount "bank"
  let (pub1, priv1) = mkAccount "account-1"
  let (pub2, priv2) = mkAccount "account-2"

  around withApp $ do
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})
    let run port act = runClientM act (clientEnv port)

    describe "" $ do
      it "fresh account should have 0 money" $ \port -> do
        res <- run port (Client.getBalance pub1)
        res `shouldBe` (Right $ Money 0)

      it "moving money increase account balance" $ \port -> do
        res <- run port $ do
          Client.addTx $ mkTx privBank pub1 100 ""
          Client.getBalance pub1
        res `shouldBe` (Right $ Money 100)

      it "insufficient funds from fresh account" $ \port -> do
        res <- run port $ do
          Client.addTx $ mkTx priv1 pub2 100 ""
        res `errorContains` "InsufficientFunds"

      it "insufficient funds from half empty account" $ \port -> do
        res <- run port $ do
          Client.addTx $ mkTx privBank pub1 50 ""
          Client.addTx $ mkTx priv1 pub2 100 ""
        res `errorContains` "InsufficientFunds"

      it "commit tx and get it back" $ \port -> do
        let tx = mkTx privBank pub1 50 ""
        res <- run port $ do
          Client.addTx tx
          Client.getTx (hashTx tx)

        res `shouldBe` Right tx

      it "get unknown tx" $ \port -> do
        let tx = mkTx privBank pub1 50 ""
        res <- run port $ Client.getTx (hashTx tx)

        res `errorContains` "UnknownTx"

      it "double spend should fail" $ \port -> do
        let tx = mkTx privBank pub1 50 ""
        res <- run port $ do
          Client.addTx tx
          Client.addTx tx

        res `errorContains` "DuplicateTx"

      it "adding tx with invalid signature should fail" $ \port -> do
        let tx =
              signTxBody priv2 $
                TxBody
                  { txb_from = pubBank,
                    txb_to = pub1,
                    txb_amount = Money 100,
                    txb_nonce = ""
                  }

        res <- run port $ Client.addTx tx

        res `errorContains` "InvalidTxSignature"

-- | Send manually composed requests.
--
-- Such tests are hard to maintain but
-- + allow sending malformed requests;
-- + help us to test how exactly derived FromJSON, ToJSON instances
--   serialize/deserialize data structures
serverSpec2 :: Spec
serverSpec2 = do
  with initApp $ do
    describe "test malformed requests" $ do
      it "not base16 encoded txId" $
        get "/tx/ololo" `shouldRespondWith` 400

      it "invalid signature" $ do
        let req = "{\"body\":{\"from\":\"bank\",\"to\":\"pedro\",\"amount\":100,\"nonce\":\"\"},\"signature\":\"signed-by-bank-ololo\"}"

        put "/tx" req `shouldRespondWith` 400

      it "missing from key" $ do
        let req = "{\"body\":{\"to\":\"pedro\",\"amount\":100,\"nonce\":\"\"},\"signature\":\"signed-by-bank-ololo\"}"

        put "/tx" req `shouldRespondWith` 400

    describe "valid raw requests" $ do
      it "valid tx" $ do
        let req = "{\"body\":{\"from\":\"bank\",\"to\":\"pedro\",\"amount\":100,\"nonce\":\"\"},\"signature\":\"signed-by-bank-94b9798e1bf6f7c46ed9e369f6f1f0eb\"}"

        put "/tx" req `shouldRespondWith` 200
