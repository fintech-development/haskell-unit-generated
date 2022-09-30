{-# LANGUAGE DeriveAnyClass #-}

module TheUnit.Model.Application.IndividualApplication where

import Control.Applicative ((<|>))
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as J
import Data.Functor.Const (Const (..))
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Lens.Micro as Lens
import qualified TheUnit.Model.Application.ApplicationStatus as ApplicationStatus
import TheUnit.Model.Application.ApplicationType (ApplicationType (..))
import TheUnit.Model.Common (DeviceFingerprint, Tags)
import TheUnit.Model.Core ((.->), _omitNulls)
import TheUnit.Model.Customer.PersonalData (Address, Agent, FullName, PhoneNumber)
import TheUnit.Model.Relationships.CustomerId (CustomerId)

data IndividualApplication = IndividualApplication
  { -- | SSN of the individual (numbers only). Either an SSN or a passport number is required.
    ssn :: !T.Text,
    -- | Passport number of the individual. Either an SSN or a passport is required.
    passport :: !(Maybe T.Text),
    -- | Required on passport only. Two letters representing the individual nationality.
    -- | ISO31661 - Alpha2 format. For more information: https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2
    nationality :: !(Maybe T.Text),
    -- | Full name of the individual.
    fullName :: !FullName,
    -- | Date only.
    -- | RFC3339 format. For more information: https://en.wikipedia.org/wiki/ISO_8601#RFCs
    dateOfBirth :: !T.Text,
    -- | Address of the individual.
    address :: !Address,
    -- | Phone of the individual.
    phone :: !PhoneNumber,
    -- | Email address of the individual.
    email :: !T.Text,
    -- | IP address of the end-customer creating the application, if specified.
    ip :: !(Maybe T.Text),
    -- | Optional. Indicates whether the individual is a sole proprietor, if specified.
    soleProprietorship :: !(Maybe Bool),
    -- | Optional. Indicates if the individual is a sole proprietor who has an Employer Identification Number, if specified.
    ein :: !(Maybe T.Text),
    -- | Optional. Indicates if the individual is a sole proprietor who is doing business under a different name, if specified.
    dba :: !(Maybe T.Text),
    -- | See [Tags](https://developers.unit.co/#tags).
    tags :: !(Maybe Tags)
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data CreateIndividualApplicationRequest = CreateIndividualApplicationRequest
  { individualApplication :: !IndividualApplication,
    -- | See [Idempotency.](https://developers.unit.co/#intro-idempotency)
    idempotencyKey :: !T.Text,
    -- | Optional. A list of device fingerprints for fraud and risk prevention [See Device Fingerprints](https://developers.unit.co/applications/#device-fingerprints).
    deviceFingerprints :: !(Maybe [DeviceFingerprint]),
    -- | Optional. See [this](https://docs.unit.co/customer-api-tokens/#customers-create-customer-bearer-token-jwt) section for more information.
    jwtSubject :: !(Maybe T.Text),
    -- | Optional. The details of the person that will act as the agent that has power of attorney.
    powerOfAttorneyAgent :: !(Maybe Agent)
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (OpenApi.ToSchema)

instance J.ToJSON CreateIndividualApplicationRequest where
  toJSON CreateIndividualApplicationRequest {individualApplication = IndividualApplication {..}, ..} =
    let attributes =
          _omitNulls
            [ "ssn" .= ssn,
              "passport" .= passport,
              "nationality" .= nationality,
              "fullName" .= fullName,
              "dateOfBirth" .= dateOfBirth,
              "address" .= address,
              "phone" .= phone,
              "email" .= email,
              "ip" .= ip,
              "soleProprietorship" .= soleProprietorship,
              "ein" .= ein,
              "dba" .= dba,
              "tags" .= tags,
              "idempotencyKey" .= idempotencyKey,
              "deviceFingerprints" .= deviceFingerprints,
              "jwtSubject" .= jwtSubject,
              "powerOfAttorneyAgent" .= powerOfAttorneyAgent
            ]
     in J.object
          [ "type" .= ApplicationType'IndividualApplication,
            "attributes" .= attributes
          ]

instance J.FromJSON CreateIndividualApplicationRequest where
  parseJSON = J.withObject "CreateIndividualApplicationRequest" \o -> do
    ApplicationType'IndividualApplication <- o .: "type"
    attributes <- o .: "attributes"
    flip (J.withObject "attributes") attributes \attrs -> do
      ssn <- attrs .: "ssn"
      passport <- attrs .:? "passport"
      nationality <- attrs .:? "nationality"
      fullName <- attrs .: "fullName"
      dateOfBirth <- attrs .: "dateOfBirth"
      address <- attrs .: "address"
      phone <- attrs .: "phone"
      email <- attrs .: "email"
      ip <- attrs .:? "ip"
      soleProprietorship <- attrs .:? "soleProprietorship"
      ein <- attrs .:? "ein"
      dba <- attrs .:? "dba"
      tags <- attrs .:? "tags"
      idempotencyKey <- attrs .: "idempotencyKey"
      deviceFingerprints <- attrs .:? "deviceFingerprints"
      jwtSubject <- attrs .:? "jwtSubject"
      powerOfAttorneyAgent <- attrs .:? "powerOfAttorneyAgent"
      let individualApplication = IndividualApplication {..}
      pure CreateIndividualApplicationRequest {..}

-- | Response from @/applications@
-- [Applications](https://docs.unit.co/applications/#response)
data IndividualApplicationResponse
  = -- | Application was Approved
    IndividualApplicationResponse'Approved IndividualApplicationApproved
  | -- | The application was denied. A Customer resource will not be created.
    IndividualApplicationResponse'Denied IndividualApplicationDenied
  | -- | The application was сanceled. A Customer resource will not be created.
    IndividualApplicationResponse'Canceled IndividualApplicationCanceled
  | -- | Certain documents are required for the process to continue. You may upload them via Upload Document.
    IndividualApplicationResponse'AwaitingDocuments IndividualApplicationAwaitingDocuments
  | IndividualApplicationResponse'PendingReview IndividualApplicationPending
  | -- | The application is being evaluated asynchronously and a result should be available shortly. Listen for webhooks (application.denied, customer.created and application.awaitingdocuments) for the final result, or periodically query the application with Get by Id).
    IndividualApplicationResponse'Pending IndividualApplicationPending
  deriving (Show, Eq, Generic)
  deriving anyclass (OpenApi.ToSchema)

instance J.FromJSON IndividualApplicationResponse where
  parseJSON = J.withObject "CreateIndividualApplicationResponse" \o ->
    do
      ApplicationType'IndividualApplication <- o .: "type"
      approved o
      <|> awaitingDocuments o
      <|> pendingReview o
      <|> pending o
      <|> denied o
      <|> canceled o
    where
      -- extract only interested fields
      approved o = do
        ApplicationStatus.Approved <- o .: "attributes" .-> "status"
        applicationId <- o .: "id"
        customer <- o .: "relationships" .-> "customer"
        let approvedResult = IndividualApplicationApproved {..}
        pure $ IndividualApplicationResponse'Approved approvedResult

      awaitingDocuments o = do
        ApplicationStatus.AwaitingDocuments <- o .: "attributes" .-> "status"
        applicationId <- o .: "id"
        pure $ IndividualApplicationResponse'AwaitingDocuments IndividualApplicationAwaitingDocuments {..}

      pendingReview o = do
        ApplicationStatus.PendingReview <- o .: "attributes" .-> "status"
        applicationId <- o .: "id"
        pure $ IndividualApplicationResponse'PendingReview IndividualApplicationPending {..}

      pending o = do
        ApplicationStatus.Pending <- o .: "attributes" .-> "status"
        applicationId <- o .: "id"
        pure $ IndividualApplicationResponse'Pending IndividualApplicationPending {..}

      denied o = do
        ApplicationStatus.Denied <- o .: "attributes" .-> "status"
        applicationId <- o .: "id"
        pure $ IndividualApplicationResponse'Denied IndividualApplicationDenied {..}

      canceled o = do
        ApplicationStatus.Canceled <- o .: "attributes" .-> "status"
        applicationId <- o .: "id"
        pure $ IndividualApplicationResponse'Canceled IndividualApplicationCanceled {..}

--

-- | Describe only interested fields
-- full description of response see:
--  - [IndividualApplication](https://docs.unit.co/resources/#individualapplication)
--  - [Applications](https://docs.unit.co/applications/#response)
data IndividualApplicationApproved = IndividualApplicationApproved
  { applicationId :: !T.Text,
    -- | @customer: created Customer in Unit
    --   customer required for create new account
    customer :: !CustomerId
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (OpenApi.ToSchema)

-- | Describe only interested fields
-- full description of response see:
--  - [IndividualApplication](https://docs.unit.co/resources/#individualapplication)
--  - [Applications](https://docs.unit.co/applications/#response)
newtype IndividualApplicationAwaitingDocuments = IndividualApplicationAwaitingDocuments
  { applicationId :: T.Text
  -- TODO(@asimuskov): add documents from included
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (OpenApi.ToSchema)

newtype IndividualApplicationPending = IndividualApplicationPending
  { -- | The application is being evaluated asynchronously and a result should be available shortly. Listen for webhooks (application.denied, customer.created and application.awaitingdocuments) for the final result, or periodically query the application with Get by Id).
    applicationId :: T.Text
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (OpenApi.ToSchema)

newtype IndividualApplicationDenied = IndividualApplicationDenied
  { applicationId :: T.Text
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (OpenApi.ToSchema)

newtype IndividualApplicationCanceled = IndividualApplicationCanceled
  { applicationId :: T.Text
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (OpenApi.ToSchema)

_applicationId :: Lens.SimpleGetter IndividualApplicationResponse T.Text
_applicationId f =
  Const . getConst . f . \case
    IndividualApplicationResponse'Approved IndividualApplicationApproved {applicationId} -> applicationId
    IndividualApplicationResponse'Denied IndividualApplicationDenied {applicationId} -> applicationId
    IndividualApplicationResponse'Canceled IndividualApplicationCanceled {applicationId} -> applicationId
    IndividualApplicationResponse'AwaitingDocuments IndividualApplicationAwaitingDocuments {applicationId} -> applicationId
    IndividualApplicationResponse'PendingReview IndividualApplicationPending {applicationId} -> applicationId
    IndividualApplicationResponse'Pending IndividualApplicationPending {applicationId} -> applicationId
