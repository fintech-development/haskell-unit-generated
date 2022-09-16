module TheUnit.Model.Relationships.AuthorizationRequestId (AuthorizationRequestId (..)) where

import qualified Data.Aeson as J
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import TheUnit.Model.Relationships.RelationshipsObject (RelationshipsObject (..))

-- | AuthorizationRequestId relationships id
-- "type" : "purchaseAuthorizationRequest"
newtype AuthorizationRequestId = AuthorizationRequestId {getAuthorizationRequestId :: T.Text}
  deriving (Show, Eq, Generic)
  deriving (OpenApi.ToSchema) via T.Text

instance J.ToJSON AuthorizationRequestId where
  toJSON (AuthorizationRequestId _id) =
    J.toJSON $ RelationshipsObject "purchaseAuthorizationRequest" _id

instance J.FromJSON AuthorizationRequestId where
  parseJSON o = do
    RelationshipsObject {..} <- J.parseJSON o
    case _type of
      "purchaseAuthorizationRequest" -> pure ()
      _ -> fail "not purchaseAuthorizationRequest"
    pure $ AuthorizationRequestId _id
