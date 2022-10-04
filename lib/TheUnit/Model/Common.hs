{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

module TheUnit.Model.Common where

import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as J
import qualified Data.Map.Strict as Map
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.Integrated.HTTP.Core (_omitNulls)
import TheUnit.Model.Orphans ()

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
  { total :: !Int,
    limit :: !Int,
    offset :: !Int
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

-- | Special tags, support kev-value map, where key is always String and value is JSON primitive: number|string|boolean
type Tags = Map.Map T.Text T.Text

data UnitJSONObject = UnitJSONObject
  { typeName :: !T.Text,
    objId :: !T.Text,
    attributes :: !J.Value,
    relationships :: !J.Value
  }

instance J.ToJSON UnitJSONObject where
  toJSON UnitJSONObject {..} =
    _omitNulls
      [ "id" .= objId,
        "type" .= typeName,
        "attributes" .= attributes,
        "relationships" .= relationships
      ]

instance J.FromJSON UnitJSONObject where
  parseJSON = J.withObject "UnitJSONObject" \o -> do
    objId <- o .: "id"
    typeName <- o .: "type"
    attributes <- o .: "attributes"
    relationships <- o .: "relationships"
    pure $ UnitJSONObject {..}

-- | HealthcareAmounts
data HealthcareAmounts = HealthcareAmounts
  { -- | Dental expense (cents).
    dentalAmount :: !Int,
    -- | Transit expense (cents).
    transitAmount :: !Int,
    -- | Vision expense (cents).
    visionOpticalAmount :: !Int,
    -- | Prescription drugs expense (cents).
    prescriptionRXAmount :: !Int,
    -- | Misc medical expense (cents).
    clinicOtherQualifiedMedicalAmount :: !Int,
    -- | Total medical expense (cents).
    totalHealthcareAmount :: !Int
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)
