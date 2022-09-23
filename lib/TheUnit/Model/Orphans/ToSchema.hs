{-# OPTIONS_GHC -Wno-orphans #-}

module TheUnit.Model.Orphans.ToSchema () where

import qualified Data.Aeson as J
import Data.OpenApi (HasDescription (description), NamedSchema (NamedSchema))
import qualified Data.OpenApi as OpenApi
import Lens.Micro ((&), (?~))
import Network.Integrated.HTTP.Core (Date, DateTime)

instance OpenApi.ToSchema Date where
  declareNamedSchema _ =
    pure $ NamedSchema (Just "Date") $ mempty & description ?~ "ISO8601 Date"

instance OpenApi.ToSchema DateTime where
  declareNamedSchema _ =
    pure $ NamedSchema (Just "DateTime") $ mempty & description ?~ "ISO8601 DateTime"

instance OpenApi.ToSchema J.Value where
  declareNamedSchema _ =
    pure $ NamedSchema (Just "json value") $ mempty & description ?~ "ANY JSON"
