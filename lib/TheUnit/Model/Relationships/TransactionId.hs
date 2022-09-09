module TheUnit.Model.Relationships.TransactionId (TransactionId (..)) where

import qualified Data.Aeson as J
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import TheUnit.Model.Relationships.RelationshipsObject (RelationshipsObject (..))

-- | TransactionId relationships id
-- "type" : "transaction"
newtype TransactionId = TransactionId {getTransactionId :: T.Text}
  deriving (Show, Eq, Generic)
  deriving (OpenApi.ToSchema) via T.Text

instance J.ToJSON TransactionId where
  toJSON (TransactionId _id) =
    J.toJSON $ RelationshipsObject "transaction" _id

instance J.FromJSON TransactionId where
  parseJSON o = do
    RelationshipsObject {..} <- J.parseJSON o
    case _type of
      "transaction" -> pure ()
      _ -> fail "not transaction"
    pure $ TransactionId _id
