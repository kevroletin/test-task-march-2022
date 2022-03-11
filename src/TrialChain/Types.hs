{-# LANGUAGE TemplateHaskell #-}

-- |
-- These types serve both as:
-- 1. part of API definition for Servant
-- 2. data types for simulator (business logic).
--
-- It's common to separate two (1) from (2). Write separate data definition for
-- *Req, *Resp types and implement conversion to internal representation. That
-- is required for two reasons:
--
-- * internal representation often doesn't match object structure in API for
--   many reasons;
--
-- * the same concept might be represented differently in *Req/*Resp (for example
--   a commited transition might have a timestamp, while request to add a
--   transation doesn't);
--
-- * API might maintain backward compatibility and different versions.
module TrialChain.Types
  ( AppError (..),
    Hash,
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

import Data.Aeson (FromJSON, ToJSON, Value (..), defaultOptions, fieldLabelModifier, parseJSON)
import Data.Aeson.TH (deriveJSON)
import Data.Binary (Binary)
import qualified Data.Text.Encoding.Base16 as Base16
import Protolude
import Servant (FromHttpApiData (..), ToHttpApiData (..))

data AppError
  = InvalidTxSignature Signature
  | InsufficientFunds {err_balance :: Money, err_amount :: Money}
  | DuplicateTx Hash
  | UnknownTx Hash
  deriving (Eq, Show, Generic)

-- | Represents Base16 encoded value
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

-- TODO: money should be non-negative
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

-- |
-- There is no enforcement on validity of signature
data Tx = Tx
  { tx_body :: TxBody,
    tx_signature :: Signature
  }
  deriving (Generic, Show, Eq, Ord, NFData)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''Tx)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''TxBody)

instance FromHttpApiData Hash where
  parseUrlPiece = mkHash

instance FromHttpApiData PublicKey where
  parseUrlPiece = second PublicKey . parseUrlPiece

instance ToHttpApiData Hash where
  toUrlPiece = unHash

instance ToHttpApiData PublicKey where
  toUrlPiece = unPublicKey
