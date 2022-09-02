{-# LANGUAGE DeriveAnyClass #-}

module TheUnit.Model.Common where

import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as J
import qualified Data.Map.Strict as Map
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import TheUnit.Model.Core ((.->))

-- SEE: more generated types here:
-- - https://github.com/unit-finance/unit-node-sdk/blob/main/types/common.ts

data BaseListParams = BaseListParams
  { -- | Maximum number of resources that will be returned. Maximum is 1000 resources. See Pagination.
    -- default: 100
    limit :: !(Maybe Int),
    -- | Number of resources to skip. See Pagination.
    -- default: 0
    offset :: !(Maybe Int)
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

-- | JSON object that contains pagination data
newtype Meta = Meta {meta :: MetaPaginationObject}
  deriving (Show, Eq, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

newtype MetaPaginationObject = MetaPaginationObject {pagination :: MetaPagination}
  deriving (Show, Eq, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data MetaPagination = MetaPagination
  { total :: Int,
    limit :: Int,
    offset :: Int
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

--

-- | More about [DeviceFingerprint](https://developers.unit.co/types#devicefingerprint)
data DeviceFingerprint = DeviceFingerprint
  { -- | Provider of the device fingerprint fraud and risk prevention. The value is always iovation
    provider :: !T.Text,
    -- | The device fingerprint blackbox value.
    value :: !T.Text
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

-- * Relationships

data RelationshipsObject = RelationshipsObject
  { _type :: !T.Text,
    _id :: !T.Text
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (OpenApi.ToSchema)

instance J.ToJSON RelationshipsObject where
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
  toJSON RelationshipsObject {..} =
    J.object ["data" .= J.object ["type" .= _type, "id" .= _id]]

instance J.FromJSON RelationshipsObject where
  parseJSON = J.withObject "RelationshipsObject" \o -> do
    _type <- o .: "data" .-> "type"
    _id <- o .: "data" .-> "id"
    pure RelationshipsObject {..}

type Tags = Map.Map T.Text T.Text
