{-# LANGUAGE TemplateHaskell #-}

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

import Control.DeepSeq
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Binary
import qualified Data.Text.Encoding.Base16 as Base16
import Protolude

newtype Hash = Hash {unHash :: Text}
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, ToJSON, NFData)

instance FromJSON Hash where
  parseJSON (String str) =
    case mkHash str of
      Left _ -> mzero
      Right x -> pure x
  parseJSON _ = mzero

mkHash :: Text -> Either Text Hash
mkHash str =
  if Base16.isBase16 str
    then Right (Hash str)
    else Left "Not a base16 encoded string"

mkHashUnsafe :: Text -> Hash
mkHashUnsafe = Hash

newtype Signature = Signature {unSignature :: Text}
  deriving stock (Generic)
  deriving newtype (Binary, Show, Eq, Ord, FromJSON, ToJSON, NFData)

newtype PublicKey = PublicKey {unPublicKey :: Text}
  deriving stock (Generic)
  deriving newtype (Binary, Show, Eq, Ord, FromJSON, ToJSON, NFData)

newtype PrivateKey = PrivateKey {unPrivateKey :: Text}
  deriving stock (Generic)
  deriving newtype (Binary, Show, Eq, Ord, FromJSON, ToJSON, NFData)

newtype Money = Money {unMoney :: Integer}
  deriving stock (Generic)
  deriving newtype (Binary, Show, Eq, Ord, FromJSON, ToJSON, NFData)

data TxBody = TxBody
  { txb_from :: PublicKey,
    txb_to :: PublicKey,
    txb_amount :: Money,
    txb_nonce :: Text
  }
  deriving (Binary, Generic, Show, Eq, Ord, NFData)

-- Invalid Tx might flow through app :(
data Tx = Tx
  { tx_body :: TxBody,
    tx_signature :: Signature
  }
  deriving (Generic, Show, Eq, Ord, NFData)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''Tx)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''TxBody)
