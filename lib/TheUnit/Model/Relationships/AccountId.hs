module TheUnit.Model.Relationships.AccountId (AccountId (..)) where

import qualified Data.Aeson as J
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import TheUnit.Model.Relationships.RelationshipsObject (RelationshipsObject (..))

-- | DepositAccountId relationships id
-- "type" : "depositAccount"
-- "type" : "account"
newtype AccountId = AccountId {getAccountId :: T.Text}
  deriving (Show, Eq, Generic)
  deriving (OpenApi.ToSchema) via T.Text

instance J.ToJSON AccountId where
  toJSON (AccountId _id) =
    J.toJSON $ RelationshipsObject "account" _id

instance J.FromJSON AccountId where
  parseJSON o = do
    RelationshipsObject {..} <- J.parseJSON o
    case _type of
      -- In AccountCreate relationship
      "depositAccount" -> pure ()
      -- In Payments relationship
      "account" -> pure ()
      _ -> fail "not depositAccount"
    pure $ AccountId _id
