module TrialChain.Signature
  ( hashStr,
    priv2pub,
    signStr,
    validateSign,
    mkAccount,
    hashTx,
    hashTxBody,
    signTxBody,
    validateTxSign,
    mkTx,
  )
where

import Crypto.Hash (MD5 (..), hashWith)
import Data.Binary (Binary, put)
import Data.Binary.Put (runPut)
import Data.ByteArray (convert)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Protolude hiding (put)
import TrialChain.Types (Hash, Money (..), PrivateKey (..), PublicKey (..), Signature (..), Tx (..), TxBody (..), mkHashUnsafe, unHash)

signMagicPrefix :: Text
signMagicPrefix = "signed-by-"

hashStr :: Text -> Hash
hashStr = mkHashUnsafe . Base16.encodeBase16 . convert . hashWith MD5 . encodeUtf8

priv2pub :: PrivateKey -> PublicKey
priv2pub (PrivateKey accName) =
  PublicKey (fromMaybe accName $ T.stripPrefix signMagicPrefix accName)

pub2privCheat :: PublicKey -> PrivateKey
pub2privCheat (PublicKey accName) =
  PrivateKey (signMagicPrefix <> accName)

signStr :: PrivateKey -> Text -> Signature
signStr (PrivateKey key) str = Signature (key <> "-" <> (unHash . hashStr) str)

validateSign :: PublicKey -> Text -> Signature -> Maybe Text
validateSign pubKey str signVal =
  if signStr (pub2privCheat pubKey) str == signVal
    then Just str
    else Nothing

mkAccount :: Text -> (PublicKey, PrivateKey)
mkAccount name = let pubKey = PublicKey name in (pubKey, pub2privCheat pubKey)

-- Confusing functions because Text also has Binary instance
serializeBin :: Binary a => a -> Text
serializeBin = decodeUtf8 . BSL.toStrict . runPut . put

-- Confusing functions because Text also has Binary instance
signBin :: Binary a => PrivateKey -> a -> Signature
signBin key = signStr key . serializeBin

hashTx :: Tx -> Hash
hashTx (Tx body _) = hashStr (serializeBin body)

hashTxBody :: TxBody -> Hash
hashTxBody body = hashStr (serializeBin body)

signTxBody :: PrivateKey -> TxBody -> Tx
signTxBody key body =
  Tx
    { tx_body = body,
      tx_signature = signBin key body
    }

validateTxSign :: Tx -> Maybe Tx
validateTxSign tx@(Tx body signVal) =
  case validateSign (txb_from body) (serializeBin body) signVal of
    Just _ -> Just tx
    Nothing -> Nothing

mkTx :: PrivateKey -> PublicKey -> Integer -> Text -> Tx
mkTx privFrom pubTo amount nonce =
  signTxBody privFrom $
    TxBody
      { txb_from = priv2pub privFrom,
        txb_to = pubTo,
        txb_amount = Money amount,
        txb_nonce = nonce
      }
