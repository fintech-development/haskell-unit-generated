module TheUnit.Model.Relationships.ApplicationId where

import qualified Data.Aeson as J
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import TheUnit.Model.Relationships.RelationshipsObject (RelationshipsObject (..))

-- | ApplicationId relationships id
-- "type" : "individualApplication"
newtype ApplicationId = ApplicationId {getApplicationId :: T.Text}
  deriving (Show, Eq, Generic)
  deriving (OpenApi.ToSchema) via T.Text

instance J.ToJSON ApplicationId where
  toJSON (ApplicationId _id) =
    J.toJSON $ RelationshipsObject "individualApplication" _id

instance J.FromJSON ApplicationId where
  parseJSON o = do
    RelationshipsObject {..} <- J.parseJSON o
    case _type of
      -- In Payments relationship
      "individualApplication" -> pure ()
      _ -> fail "not individualApplication"
    pure $ ApplicationId _id
