{-# LANGUAGE DeriveAnyClass #-}

module TheUnit.Model.Response where

import Control.Applicative ((<|>))
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as J
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import TheUnit.Model.Core (_omitNulls)

-- | Used to wrap all Unit request payloads
-- Additionally used in relationships objects and
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

data UnitResponse a = UnitErrorResponse (NE.NonEmpty UnitErrorPayload) | UnitResponseData a
  deriving (Show, Eq, Generic)

-- | FromJSON UnitResponse
instance (J.FromJSON a) => J.FromJSON (UnitResponse a) where
  parseJSON = J.withObject "UnitResponse" $ \o ->
    errors o <|> responseData o
    where
      errors o = do
        errors_ <- o .: "errors"
        pure $ UnitErrorResponse errors_
      responseData o = do
        data_ <- o .: "data"
        pure $ UnitResponseData data_

-- | ToJSON UnitResponse
instance (J.ToJSON a) => J.ToJSON (UnitResponse a) where
  toJSON (UnitErrorResponse errors) =
    J.object ["errors" .= errors]
  toJSON (UnitResponseData responseData) =
    J.object ["data" .= responseData]

-- | ToSchema UnitResponse
instance OpenApi.ToSchema a => OpenApi.ToSchema (UnitResponse a)

-- https://docs.unit.co/#intro-errors
data UnitErrorPayload = UnitErrorPayload
  { status :: !T.Text, -- http status code
    title :: !T.Text,
    code :: !(Maybe T.Text),
    details :: !(Maybe T.Text),
    meta :: !(Maybe (Map.Map T.Text T.Text))
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (OpenApi.ToSchema)

-- | FromJSON UnitResponse
instance J.FromJSON UnitErrorPayload where
  parseJSON = J.withObject "UnitErrorPayload" $ \o -> do
    status <- (o .: "status") <|> (o .: "status")
    title <- o .: "title"
    code <- o .:? "code"
    details <- o .:? "details"
    meta <- o .:? "meta"

    pure UnitErrorPayload {..}

-- | ToJSON UnitResponse
instance J.ToJSON UnitErrorPayload where
  toJSON UnitErrorPayload {..} =
    _omitNulls
      [ "status" .= status,
        "title" .= title,
        "code" .= code,
        "details" .= details,
        "meta" .= meta
      ]
