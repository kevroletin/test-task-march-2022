module TrialChain.Types where

import Data.Binary
import Protolude

newtype Hash = Hash {unHash :: ByteString} deriving (Show, Eq, Ord)

newtype Signature = Signature {unSignature :: ByteString}
  deriving stock (Generic)
  deriving newtype (Binary, Show, Eq, Ord)

newtype PublicKey = PublicKey {unPublicKey :: ByteString}
  deriving stock (Generic)
  deriving newtype (Binary, Show, Eq, Ord)

newtype PrivateKey = PrivateKey {unPrivateKey :: ByteString}
  deriving stock (Generic)
  deriving newtype (Binary, Show, Eq, Ord)

newtype Money = Money {unMoney :: Integer}
  deriving stock (Generic)
  deriving newtype (Binary, Show, Eq, Ord)

data TxBody = TxBody
  { txb_from :: PublicKey,
    txb_to :: PublicKey,
    txb_amount :: Money,
    txb_nonce :: ByteString
  }
  deriving (Binary, Generic, Show, Eq, Ord)

-- Invalid Tx might flow through app :(
data Tx = Tx
  { tx_body :: TxBody,
    tx_signature :: Signature
  }
  deriving (Generic, Show, Eq, Ord)
