{-# LANGUAGE DeriveAnyClass #-}

module TheUnit.Model.Relationships.RelationshipsObject (RelationshipsObject (..)) where

import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as J
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import TheUnit.Model.Core ((.->))

-- * Relationships

-- Any Object inside "relationships"
--
-- Example:
-- "relationships": {
--          "org": {
--              "data": {
--                  "type": "org",
--                  "id": "1988"
--              }
--          },
--          "customer": {
--              "data": {
--                  "type": "individualCustomer",
--                  "id": "515994"
--              }
--          }
--      }
data RelationshipsObject = RelationshipsObject
  { _type :: !T.Text,
    _id :: !T.Text
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (OpenApi.ToSchema)

instance J.ToJSON RelationshipsObject where
  toJSON RelationshipsObject {..} =
    J.object ["data" .= J.object ["type" .= _type, "id" .= _id]]

instance J.FromJSON RelationshipsObject where
  parseJSON = J.withObject "RelationshipsObject" \o -> do
    _type <- o .: "data" .-> "type"
    _id <- o .: "data" .-> "id"
    pure RelationshipsObject {..}
