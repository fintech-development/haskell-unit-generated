{-# LANGUAGE DeriveAnyClass #-}

module TheUnit.Model.Accounts.DepositAccount where

import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as J
import Data.Aeson.Deriving
import Data.Generics.Labels ()
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import Network.Integrated.HTTP.Core (DateTime)
import TheUnit.Model.Accounts.UnitDepositProduct (UnitDepositProduct)
import TheUnit.Model.Common (Tags)
import TheUnit.Model.Core ((.->), _omitNulls)
import TheUnit.Model.Customer.IndividualCustomer (IndividualCustomerId)
import TheUnit.Model.Orphans ()

-- | [depositAccount](https://docs.unit.co/resources#depositaccount)
data UnitDepositAccount = UnitDepositAccount
  { -- | Identifier of the deposit account resource.
    depositAccountId :: !T.Text,
    --

    -- | The customer.
    -- (@asimuskov) Now support only one customer on account
    -- but Unit can support multiple customers
    -- SEE: [depositoryAccount relationships](https://docs.unit.co/resources#relationships-8)
    customer :: !IndividualCustomerId,
    --

    -- | The date the resource was created. RFC3339 Date string
    createdAt :: !DateTime,
    -- | The date the resource was updated. RFC3339 Date string. Optional.
    updateAt :: !(Maybe DateTime),
    -- | Name of the account holder.
    name :: !T.Text,
    -- | Status of the account, either Open, Frozen, or Closed.
    status :: !UnitDepositAccountStatus,
    -- | The name of the deposit product.
    -- [deposit product](https://docs.unit.co/deposit-accounts#deposit-products)
    depositProduct :: !UnitDepositProduct,
    -- | Currency of the account.
    currency :: !T.Text,
    -- | The balance amount (in cents). The balance represents the funds that are currently in the account (not taking into account future commitments). The balance equals the sum of 'available' and 'hold'.
    balance :: !Int,
    -- | The hold amount (in cents). The hold represents funds that are not available for spending, due to an outstanding card authorization.
    hold :: !Int,
    -- | The available balance for spending (in cents). Equals the balance minus the hold amount.
    available :: !Int,
    -- | Tags
    tags :: !(Maybe Tags),
    --

    ach :: !DepositoryAccountACH,
    --

    -- | The reason the account was frozen, either Fraud or free-text description. Optional.
    freezeReason :: !(Maybe T.Text),
    -- | The reason the account was closed, either ByCustomer or Fraud. Optional.
    closeReason :: !(Maybe T.Text),
    -- | The expanded fraud reason for closing the account when Fraud is specified as the reason. Optional.
    --
    -- Can be one of: (ACHActivity, CardActivity, CheckActivity, ApplicationHistory, AccountActivity, ClientIdentified, IdentityTheft, LinkedToFraudulentCustomer).
    fraudReason :: !(Maybe T.Text),
    --

    -- | The account DACA (Deposit Account Control Agreements) status. Can be one of: Entered, Activated. Optional.
    dacaStatus :: !(Maybe T.Text)
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (OpenApi.ToSchema)

instance J.FromJSON UnitDepositAccount where
  parseJSON = J.withObject "UnitDepositAccount" \o -> do
    "depositAccount" :: T.Text <- o .: "type"
    depositAccountId <- o .: "id"
    customer <- o .: "relationships" .-> "customer"

    attributes <- o .: "attributes"

    createdAt <- attributes .: "createdAt"
    updateAt <- attributes .:? "updateAt"
    name <- attributes .: "name"
    status <- attributes .: "status"
    depositProduct <- attributes .: "depositProduct"
    currency <- attributes .: "currency"
    balance <- attributes .: "balance"
    hold <- attributes .: "hold"
    available <- attributes .: "available"
    tags <- attributes .:? "tags"
    freezeReason <- attributes .:? "freezeReason"
    closeReason <- attributes .:? "closeReason"
    fraudReason <- attributes .:? "fraudReason"
    dacaStatus <- attributes .:? "dacaStatus"

    routingNumber <- attributes .: "routingNumber"
    accountNumber <- attributes .: "accountNumber"
    let ach = DepositoryAccountACH {..}
    pure UnitDepositAccount {..}

instance J.ToJSON UnitDepositAccount where
  toJSON UnitDepositAccount {..} =
    let attributes =
          _omitNulls
            [ "name" .= name,
              "createdAt" .= createdAt,
              "updateAt" .= updateAt,
              "routingNumber" .= (ach ^. #routingNumber),
              "accountNumber" .= (ach ^. #accountNumber),
              "depositProduct" .= depositProduct,
              "balance" .= balance,
              "hold" .= hold,
              "available" .= available,
              "currency" .= currency,
              "status" .= status,
              "tags" .= tags,
              "freezeReason" .= freezeReason,
              "closeReason" .= closeReason,
              "fraudReason" .= fraudReason,
              "dacaStatus" .= dacaStatus
            ]
     in _omitNulls
          [ "type" .= ("depositAccount" :: T.Text),
            "id" .= depositAccountId,
            "attributes" .= attributes,
            "relationships" .= _omitNulls ["customer" .= J.toJSON customer]
          ]

data DepositoryAccountACH = DepositoryAccountACH
  { -- | Routing number of account
    routingNumber :: !T.Text,
    -- | Account number, together with the routingNumber forms the identifier of the account on the ACH network.
    accountNumber :: !T.Text
  }
  deriving (Eq, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

instance Show DepositoryAccountACH where
  show DepositoryAccountACH {} = "DepositoryAccountACH {routingNumber = <HIDDEN>, accountNumber = <HIDDEN>}"

data UnitDepositAccountStatus
  = UnitDepositAccountStatus'Open
  | UnitDepositAccountStatus'Closed
  | UnitDepositAccountStatus'Frozen
  deriving (Show, Eq, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving
    (J.ToJSON, J.FromJSON)
    via GenericEncoded
          '[ConstructorTagModifier := DropPrefix "UnitDepositAccountStatus'"]
          UnitDepositAccountStatus

data CreateDepositAccountData = CreateDepositAccountData
  { customer :: !IndividualCustomerId,
    depositProduct :: !UnitDepositProduct,
    idempotencyKey :: !T.Text,
    tags :: !(Maybe Tags)
  }

instance J.ToJSON CreateDepositAccountData where
  toJSON CreateDepositAccountData {..} =
    _omitNulls
      [ "type" .= ("depositAccount" :: T.Text),
        "attributes"
          .= _omitNulls
            [ "depositProduct" .= depositProduct,
              "tags" .= tags,
              "idempotencyKey" .= idempotencyKey
            ],
        "relationships"
          .= _omitNulls
            ["customer" .= customer]
      ]

instance J.FromJSON CreateDepositAccountData where
  parseJSON = J.withObject "CreateDepositAccountData" \o -> do
    "depositAccount" :: T.Text <- o .: "type"

    attributes <- o .: "attributes"
    customer <- o .: "relationships" .-> "customer"

    depositProduct <- attributes .: "depositProduct"
    tags <- attributes .: "tags"
    idempotencyKey <- attributes .: "idempotencyKey"

    pure CreateDepositAccountData {..}
