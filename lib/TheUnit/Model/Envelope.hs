module TheUnit.Model.Envelope (UnitEnvelope (..)) where

import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as J
import qualified Data.OpenApi as OpenApi
import GHC.Generics (Generic)

-- | Wrapper around all Unit request payloads
newtype UnitEnvelope a = UnitEnvelope {payload :: a}
  deriving (Show, Eq, Generic)

-- | FromJSON UnitResponse
instance (J.FromJSON a) => J.FromJSON (UnitEnvelope a) where
  parseJSON = J.withObject "UnitEnvelope" $ \o ->
    UnitEnvelope <$> o .: "data"

-- | ToJSON UnitResponse
instance (J.ToJSON a) => J.ToJSON (UnitEnvelope a) where
  toJSON (UnitEnvelope {payload}) =
    J.object ["data" .= payload]

-- | ToSchema UnitResponse
instance OpenApi.ToSchema a => OpenApi.ToSchema (UnitEnvelope a)
