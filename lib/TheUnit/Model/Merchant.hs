{-# LANGUAGE DeriveAnyClass #-}

module TheUnit.Model.Merchant (Merchant (..)) where

import Data.Aeson ((.:?), (.=))
import qualified Data.Aeson as J
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.Integrated.HTTP.Core (_omitNulls)

data Merchant = Merchant
  { -- | The name of the merchant.
    name :: !(Maybe T.Text),
    -- | The 4-digit ISO 18245 merchant category code (MCC).
    merchantType :: !(Maybe Int),
    -- | The merchant category, described by the MCC code (see [this reference](https://github.com/greggles/mcc-codes) for the list of category descriptions).
    category :: !(Maybe T.Text),
    -- | Optional. The location (city, state, etc.) of the merchant.
    location :: !(Maybe T.Text),
    -- | Optional. The unique network merchant identifier.
    merchantId :: !(Maybe T.Text)
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)

instance J.ToJSON Merchant where
  toJSON Merchant {..} =
    _omitNulls
      [ "name" .= name,
        "type" .= merchantType,
        "category" .= category,
        "location" .= location,
        "id" .= merchantId
      ]

instance J.FromJSON Merchant where
  parseJSON = J.withObject "Merchant" \o -> do
    name <- o .:? "name"
    merchantType <- o .:? "type"
    category <- o .:? "category"
    location <- o .:? "location"
    merchantId <- o .:? "id"
    pure Merchant {..}
