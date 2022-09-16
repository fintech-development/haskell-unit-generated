module TheUnit.Model.Relationships.DocumentId (DocumentId (..)) where

import qualified Data.Aeson as J
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import TheUnit.Model.Relationships.RelationshipsObject (RelationshipsObject (..))

-- | DocumentId relationships id
newtype DocumentId = DocumentId {getDocumentId :: T.Text}
  deriving (Show, Eq, Generic)
  deriving (OpenApi.ToSchema) via T.Text

instance J.ToJSON DocumentId where
  toJSON (DocumentId _id) =
    J.toJSON $ RelationshipsObject "document" _id

instance J.FromJSON DocumentId where
  parseJSON o = do
    RelationshipsObject {..} <- J.parseJSON o
    case _type of
      "document" -> pure ()
      _ -> fail "not document"
    pure $ DocumentId _id
