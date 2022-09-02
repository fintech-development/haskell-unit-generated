{-# LANGUAGE DeriveAnyClass #-}

module TheUnit.Model.Customer.IndividualCustomer (IndividualCustomerId (..)) where

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
    J.toJSON $ RelationshipsObject "customer" _id

instance J.FromJSON IndividualCustomerId where
  parseJSON o = do
    RelationshipsObject {..} <- J.parseJSON o
    case _type of
      -- `individualCustomer` used in Application API
      "individualCustomer" -> pure ()
      -- `customer` user in Accounts API
      "customer" -> pure ()
      _ -> fail "not individualCustomer"
    pure $ IndividualCustomerId _id
