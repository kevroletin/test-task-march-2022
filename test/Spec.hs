import Protolude
import Test.Hspec

import TrialChain.Signature
import TrialChain.Types

main :: IO ()
main = hspec $ do
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

    let (pubFrom, privFrom) = mkAccount "testAccFrom"
    let (pubTo, privTo) = mkAccount "testAccTo"

    it "sign and check tx signature" $ do
      let txB = TxBody { txb_from = pubFrom
                       , txb_to = pubTo
                       , txb_amount = Money 100
                       , txb_nonce = ""
                       }
      let tx = signTxBody privFrom txB
      validateTxSign tx `shouldBe` Just tx

    it "sign wrong tx signature" $ do
      let txB = TxBody { txb_from = pubFrom
                       , txb_to = pubTo
                       , txb_amount = Money 100
                       , txb_nonce = ""
                       }
      let tx = signTxBody privTo txB
      validateTxSign tx `shouldBe` Nothing
