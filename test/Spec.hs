import Protolude
import Test.Hspec
import TrialChain.AppState
import TrialChain.Signature
import TrialChain.Types

main :: IO ()
main = hspec $ do
  let (pubFrom, privFrom) = mkAccount "testAccFrom"
  let (pubTo, privTo) = mkAccount "testAccTo"
  let (pub1, priv1) = mkAccount "testAcc1"
  let (pub2, priv2) = mkAccount "testAcc2"
  let (pub3, priv3) = mkAccount "testAcc3"

  describe "Signature" $ do
    it "create account" $ do
      let (pub, priv) = mkAccount "testAcc"
      priv2pub priv `shouldBe` pub

    it "sign and check signature" $ do
      let (pub, priv) = mkAccount "testAcc"
      let str :: ByteString = "content"
      validateSign pub str (signStr priv str) `shouldBe` Just str

    it "sign and check wrong signature" $ do
      let (pub1, priv1) = mkAccount "testAcc1"
      let (pub2, priv2) = mkAccount "testAcc2"
      let str :: ByteString = "content"
      validateSign pub2 str (signStr priv1 str) `shouldBe` Nothing

    it "sign and check tx signature" $ do
      let txB =
            TxBody
              { txb_from = pubFrom,
                txb_to = pubTo,
                txb_amount = Money 100,
                txb_nonce = ""
              }
      let tx = signTxBody privFrom txB
      validateTxSign tx `shouldBe` Just tx

    it "sign wrong tx signature" $ do
      let txB =
            TxBody
              { txb_from = pubFrom,
                txb_to = pubTo,
                txb_amount = Money 100,
                txb_nonce = ""
              }
      let tx = signTxBody privTo txB
      validateTxSign tx `shouldBe` Nothing

  describe "tx processing" $ do
    it "move from 0 to 0" $ do
      let tx =
            signTxBody privFrom $
              TxBody
                { txb_from = pubFrom,
                  txb_to = pubTo,
                  txb_amount = Money 100,
                  txb_nonce = ""
                }
      let st = mkState [(pubTo, 100)]
      addTx tx st `shouldBe` Left (InsufficientFunds (Money 0) (Money 100))

    it "move and get balance" $ do
      let tx =
            signTxBody privFrom $
              TxBody
                { txb_from = pubFrom,
                  txb_to = pubTo,
                  txb_amount = Money 100,
                  txb_nonce = ""
                }
      let st = mkState [(pubFrom, 100)]
      let res = addTx tx st
      isRight res `shouldBe` True

      let (Right st1) = res
      getBalance pubFrom st1 `shouldBe` Money 0
      getBalance pubTo st1 `shouldBe` Money 100

    let mkTx privFrom pubTo amount nonce =
          signTxBody privFrom $
            TxBody
              { txb_from = priv2pub privFrom,
                txb_to = pubTo,
                txb_amount = Money amount,
                txb_nonce = nonce
              }

    it "move/get" $ do
      let st = mkState [(pub1, 50), (pub2, 50)]
      let res = flip evalAppM st $ do
            addTxM $ mkTx priv1 pub3 50 ""
            addTxM $ mkTx priv2 pub3 50 ""
            getBalanceM pub3
      res `shouldBe` Right (Money 100)

    it "execute the same tx twice" $ do
      let st = mkState [(pub1, 100)]
      let tx = mkTx priv1 pub2 50 ""
      let res = flip evalAppM st $ do
            addTxM tx
            addTxM tx
            pure ()
      res `shouldBe` Left (DuplicateTx (hashTx tx))

    it "execute similar tx" $ do
      let st = mkState [(pub1, 100)]
      let res = flip evalAppM st $ do
            addTxM $ mkTx priv1 pub2 50 "nonce-1"
            addTxM $ mkTx priv1 pub2 50 "nonce-2"
            getBalanceM pub2
      res `shouldBe` Right (Money 100)

    it "use wrong signature" $ do
      let st = mkState [(pub1, 100)]
      let tx =
            signTxBody
              priv2
              TxBody
                { txb_from = pub1,
                  txb_to = pub2,
                  txb_amount = Money 1,
                  txb_nonce = ""
                }
      addTx tx st `shouldBe` Left (InvalidTxSignature (tx_signature tx))

    it "get unknown tx" $ do
      let st = mkState []
      let tx = mkTx priv1 pub2 100 ""
      getTx (hashTx tx) st `shouldBe` Left (UnknownTx (hashTx tx))
