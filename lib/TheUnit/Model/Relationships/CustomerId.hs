module TheUnit.Model.Relationships.CustomerId (CustomerId (..)) where

import qualified Data.Aeson as J
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import TheUnit.Model.Relationships.RelationshipsObject (RelationshipsObject (..))

-- | CustomerId relationships id
newtype CustomerId = CustomerId {getCustomerId :: T.Text}
  deriving (Show, Eq, Generic)
  deriving (OpenApi.ToSchema) via T.Text

instance J.ToJSON CustomerId where
  toJSON (CustomerId _id) =
    J.toJSON $ RelationshipsObject "customer" _id

instance J.FromJSON CustomerId where
  parseJSON o = do
    RelationshipsObject {..} <- J.parseJSON o
    case _type of
      -- `individualCustomer` used in Application API
      "individualCustomer" -> pure ()
      -- `customer` user in Accounts API
      "customer" -> pure ()
      _ -> fail "not individualCustomer"
    pure $ CustomerId _id
