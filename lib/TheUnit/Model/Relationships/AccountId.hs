module TheUnit.Model.Relationships.AccountId (AccountId (..)) where

import qualified Data.Aeson as J
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import TheUnit.Model.Relationships.RelationshipsObject (RelationshipsObject (..))

-- | AccountId relationships id
-- "type" : "account"
newtype AccountId = AccountId {getAccountId :: T.Text}
  deriving (Show, Eq, Generic)
  deriving (OpenApi.ToSchema) via T.Text

instance J.ToJSON AccountId where
  toJSON (AccountId _id) =
    J.toJSON $ RelationshipsObject "depositAccount" _id

instance J.FromJSON AccountId where
  parseJSON o = do
    RelationshipsObject {..} <- J.parseJSON o
    case _type of
      -- In Payments relationship
      "account" -> pure ()
      "depositAccount" -> pure ()
      _ -> fail "not account"
    pure $ AccountId _id
