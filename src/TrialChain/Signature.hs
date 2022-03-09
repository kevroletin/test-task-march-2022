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
  )
where

import Crypto.Hash
import Data.Binary
import Data.Binary.Put (runPut)
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Protolude hiding (put)
import TrialChain.Types

signMagicPrefix :: ByteString
signMagicPrefix = "signed-by-"

hashStr :: ByteString -> Hash
hashStr = Hash . Base16.encode . convert . hashWith MD5

priv2pub :: PrivateKey -> PublicKey
priv2pub (PrivateKey accName) =
  PublicKey (fromMaybe accName $ BS.stripPrefix signMagicPrefix accName)

pub2privCheat :: PublicKey -> PrivateKey
pub2privCheat (PublicKey accName) =
  PrivateKey (signMagicPrefix <> accName)

signStr :: PrivateKey -> ByteString -> Signature
signStr (PrivateKey key) str = Signature (key <> "-" <> (unHash . hashStr) str)

validateSign :: PublicKey -> ByteString -> Signature -> Maybe ByteString
validateSign pubKey str signVal =
  if signStr (pub2privCheat pubKey) str == signVal
    then Just str
    else Nothing

mkAccount :: ByteString -> (PublicKey, PrivateKey)
mkAccount name = let pubKey = PublicKey name in (pubKey, pub2privCheat pubKey)

-- Confusing functions because ByteString also has Binary instance
serializeBin :: Binary a => a -> ByteString
serializeBin = BSL.toStrict . runPut . put

-- Confusing functions because ByteString also has Binary instance
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
