module TheUnit.Model.Relationships.CardId (CardId (..)) where

import qualified Data.Aeson as J
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import TheUnit.Model.Relationships.RelationshipsObject (RelationshipsObject (..))

-- | CardId relationships id
newtype CardId = CardId {getCardId :: T.Text}
  deriving (Show, Eq, Generic)
  deriving (OpenApi.ToSchema) via T.Text

instance J.ToJSON CardId where
  toJSON (CardId _id) =
    J.toJSON $ RelationshipsObject "card" _id

instance J.FromJSON CardId where
  parseJSON o = do
    RelationshipsObject {..} <- J.parseJSON o
    case _type of
      "card" -> pure ()
      _ -> fail "not card"
    pure $ CardId _id
