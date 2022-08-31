{-# LANGUAGE DeriveAnyClass #-}

module TheUnit.Model.Customer.IndividualCustomer (IndividualCustomerId (..)) where

import Data.Aeson ((.:))
import qualified Data.Aeson as J
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import TheUnit.Model.Common (RelationshipsObject (..))

-- | IndividualCustomerId relationships id
newtype IndividualCustomerId = IndividualCustomerId {getIndividualCustomerId :: T.Text}
  deriving (Show, Eq, Generic)
  deriving anyclass (OpenApi.ToSchema)

instance J.ToJSON IndividualCustomerId where
  toJSON (IndividualCustomerId _id) =
    J.toJSON $ RelationshipsObject "individualCustomer" _id

instance J.FromJSON IndividualCustomerId where
  parseJSON = J.withObject "IndividualCustomer" \o -> do
    _type <- o .: "type"
    flip (J.withText "type") _type \case
      "individualCustomer" -> pure ()
      _ -> fail "not individualCustomer"
    IndividualCustomerId <$> o .: "id"
