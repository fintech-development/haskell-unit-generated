{-# LANGUAGE DeriveAnyClass #-}

module TheUnit.Model.Customer.PersonalData where

import qualified Data.Aeson as J
import Data.Aeson.Deriving
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.Integrated.HTTP.Core (Date)
import TheUnit.Model.Orphans ()

data FullName = FullName
  { -- | 	Individual first name.
    first :: !T.Text,
    -- | 	Individual last name.
    last :: !T.Text
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data PhoneNumber = PhoneNumber
  { -- | Country code of the phone number.
    countryCode :: !T.Text,
    -- | The phone number without the country code.
    number :: !T.Text
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data Address = Address
  { -- | First line of an address.
    street :: !T.Text,
    -- | Optional. Second line of an address.
    street2 :: !(Maybe T.Text),
    -- | City
    city :: !T.Text,
    -- | Two letters representing the state. Only required for US addresses.
    state :: !(Maybe T.Text),
    -- | Postal code.
    postalCode :: !T.Text,
    -- | Two letters representing the country.
    -- | ISO31661 - Alpha2 format. For more information: https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2
    country :: !T.Text -- "US"
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data BaseContactAttributes = BaseContactAttributes
  { fullName :: !FullName,
    ssn :: !(Maybe T.Text),
    -- | Date only. RFC3339 format. For more information: https://en.wikipedia.org/wiki/ISO_8601#RFCs
    dateOfBirth :: !Date,
    address :: !Address,
    phone :: !PhoneNumber,
    email :: !T.Text
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data AuthorizedUser = AuthorizedUser
  { fullName :: !FullName,
    phone :: !PhoneNumber,
    email :: !T.Text
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data ContactStatus
  = Contact'Approved
  | Contact'Denied
  | Contact'PendingReview
  deriving (Show, Eq, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving
    (J.ToJSON, J.FromJSON)
    via GenericEncoded
          '[ConstructorTagModifier := DropPrefix "Contact'"]
          ContactStatus

data Agent = Agent
  { fullName :: !FullName,
    ssn :: !(Maybe T.Text),
    dateOfBirth :: !Date,
    address :: !Address,
    phone :: !PhoneNumber,
    email :: !T.Text,
    -- | One of Approved, Denied or PendingReview.
    status :: !ContactStatus,
    -- | Passport of the agent. One of ssn or passport is required.
    passport :: !(Maybe T.Text),
    -- | ISO31661-Alpha2 T.Text	Only when Passport is populated. Two letters representing the agent's nationality.
    nationality :: !(Maybe T.Text),
    -- | Optional. See [this](https://docs.unit.co/customer-api-tokens/#customers-create-customer-bearer-token-jwt) section for more information.
    jwtSubject :: !(Maybe T.Text)
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)
