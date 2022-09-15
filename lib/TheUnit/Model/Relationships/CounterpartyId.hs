module TheUnit.Model.Relationships.CounterpartyId (CounterpartyId (..)) where

import qualified Data.Aeson as J
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import TheUnit.Model.Relationships.RelationshipsObject (RelationshipsObject (..))

-- | CounterpartyId relationships id
-- "type" : "counterparty"
newtype CounterpartyId = CounterpartyId {getCounterpartyId :: T.Text}
  deriving (Show, Eq, Generic)
  deriving (OpenApi.ToSchema) via T.Text

instance J.ToJSON CounterpartyId where
  toJSON (CounterpartyId _id) =
    J.toJSON $ RelationshipsObject "counterparty" _id

instance J.FromJSON CounterpartyId where
  parseJSON o = do
    RelationshipsObject {..} <- J.parseJSON o
    case _type of
      -- In Payments relationship
      "counterparty" -> pure ()
      _ -> fail "not counterparty"
    pure $ CounterpartyId _id
