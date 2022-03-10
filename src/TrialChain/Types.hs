module TrialChain.Types
  ( Hash,
    mkHash,
    mkHashUnsafe,
    unHash,
    Signature (..),
    PublicKey (..),
    PrivateKey (..),
    Money (..),
    TxBody (..),
    Tx (..),
  )
where

import Data.Binary
import qualified Data.Text.Encoding.Base16 as Base16
import Protolude

newtype Hash = Hash {unHash :: Text} deriving (Show, Eq, Ord)

mkHash :: Text -> Either Text Hash
mkHash str =
  if Base16.isBase16 str
    then Right (Hash str)
    else Left "Not a base16 encoded string"

mkHashUnsafe :: Text -> Hash
mkHashUnsafe = Hash

newtype Signature = Signature {unSignature :: Text}
  deriving stock (Generic)
  deriving newtype (Binary, Show, Eq, Ord)

newtype PublicKey = PublicKey {unPublicKey :: Text}
  deriving stock (Generic)
  deriving newtype (Binary, Show, Eq, Ord)

newtype PrivateKey = PrivateKey {unPrivateKey :: Text}
  deriving stock (Generic)
  deriving newtype (Binary, Show, Eq, Ord)

newtype Money = Money {unMoney :: Integer}
  deriving stock (Generic)
  deriving newtype (Binary, Show, Eq, Ord)

data TxBody = TxBody
  { txb_from :: PublicKey,
    txb_to :: PublicKey,
    txb_amount :: Money,
    txb_nonce :: Text
  }
  deriving (Binary, Generic, Show, Eq, Ord)

-- Invalid Tx might flow through app :(
data Tx = Tx
  { tx_body :: TxBody,
    tx_signature :: Signature
  }
  deriving (Generic, Show, Eq, Ord)
