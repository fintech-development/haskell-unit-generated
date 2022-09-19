-- NOTE:
-- All types presented here are written with hands
-- Therefore, here may contain errors and inconsistencies with real API
-- Moreover, the description of the API in DOC or Typescript module
-- may differ from the actual API in answers
-- [Webhooks Events](https://docs.unit.co/events)
-- [TypeScript](https://github.com/unit-finance/unit-node-sdk/blob/main/types/events.ts)
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

module TheUnit.Model.Webhooks.WebhookPayload where

import qualified Data.Aeson as J
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.Integrated.HTTP.Core (Date, DateTime)
import TheUnit.Model.Common (HealthcareAmounts, Tags)
import TheUnit.Model.Merchant (Merchant)
import TheUnit.Model.Orphans ()
import TheUnit.Model.Payment
  ( Direction,
    PaymentStatus,
    ReceivedPaymentStatus,
    RecurringPaymentStatus,
  )
import TheUnit.Model.Relationships
  ( AccountId,
    ApplicationId,
    AuthorizationRequestId,
    BulkPaymentsId,
    CardId,
    CustomerId,
    DocumentId,
    PaymentId,
    ReceivedPaymentId,
    RecurringPaymentId,
    TransactionId,
  )
import TheUnit.Model.Relationships.RelationshipsObject (RelationshipsObject)
import Prelude hiding (error, id)

-- * Account

data BaseEventAttributes = BaseEventAttributes
  { createdAt :: DateTime,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data BaseEventRelationships = BaseEventRelationships
  { account :: AccountId,
    customer :: CustomerId
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data AccountCreated = AccountCreated
  { id :: T.Text,
    attributes :: BaseEventAttributes,
    relationships :: BaseEventRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data CloseReason = CloseReason
  { closeReason :: T.Text,
    createdAt :: DateTime,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data AccountClosed = AccountClosed
  { id :: T.Text,
    attributes :: CloseReason,
    relationships :: BaseEventRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data FreezeReason = FreezeReason
  { freezeReason :: T.Text,
    createdAt :: DateTime,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data AccountFrozen = AccountFrozen
  { id :: T.Text,
    attributes :: FreezeReason,
    relationships :: BaseEventRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data AccountReopened = AccountReopened
  { id :: T.Text,
    attributes :: BaseEventAttributes,
    relationships :: BaseEventRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data AccountUnfrozen = AccountUnfrozen
  { id :: T.Text,
    attributes :: BaseEventAttributes,
    relationships :: BaseEventRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

-- * Application

newtype ApplicationRelationship = ApplicationRelationship {application :: ApplicationId}
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data ApplicationCanceled = ApplicationCanceled
  { id :: T.Text,
    attributes :: BaseEventAttributes,
    relationships :: ApplicationRelationship
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data ApplicationDenied = ApplicationDenied
  { id :: T.Text,
    attributes :: BaseEventAttributes,
    relationships :: ApplicationRelationship
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data ApplicationAwaitingDocuments = ApplicationAwaitingDocuments
  { id :: T.Text,
    attributes :: BaseEventAttributes,
    relationships :: ApplicationRelationship
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data ApplicationPendingReview = ApplicationPendingReview
  { id :: T.Text,
    attributes :: BaseEventAttributes,
    relationships :: ApplicationRelationship
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

-- * Authorization

data AuthorizationRelationships = AuthorizationRelationships
  { account :: AccountId,
    customer :: CustomerId,
    authorization :: CardId,
    card :: CustomerId
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data AuthorizationAttributes = AuthorizationAttributes
  { amount :: Int,
    cardLast4Digits :: T.Text,
    recurring :: Bool,
    createdAt :: DateTime,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data AuthorizationCreatedAttributes = AuthorizationCreatedAttributes
  { amount :: Int,
    cardLast4Digits :: T.Text,
    recurring :: Bool,
    merchant :: Merchant,
    createdAt :: DateTime,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data AuthorizationCreated = AuthorizationCreated
  { id :: T.Text,
    attributes :: AuthorizationCreatedAttributes,
    relationships :: AuthorizationRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data AuthorizationCanceled = AuthorizationCanceled
  { id :: T.Text,
    attributes :: !AuthorizationAttributes,
    relationships :: !AuthorizationRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data AuthorizationDeclinedAttributes = AuthorizationDeclinedAttributes
  { amount :: Int,
    cardLast4Digits :: T.Text,
    recurring :: Bool,
    reason :: T.Text,
    createdAt :: DateTime,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data AuthorizationDeclined = AuthorizationDeclined
  { id :: T.Text,
    attributes :: AuthorizationDeclinedAttributes,
    relationships :: AuthorizationRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data AuthorizationAmountChangedAttributes = AuthorizationAmountChangedAttributes
  { oldAmount :: Int,
    newAmount :: Int,
    createdAt :: DateTime,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data AuthorizationAmountChanged = AuthorizationAmountChanged
  { id :: T.Text,
    attributes :: AuthorizationAmountChangedAttributes,
    relationships :: AuthorizationRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

-- * Authorization Request

data AuthorizationRequestRelationships = AuthorizationRequestRelationships
  { account :: AccountId,
    customer :: CustomerId,
    authorizationRequest :: AuthorizationRequestId,
    card :: CardId
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data AuthorizationRequestAttributes = AuthorizationRequestAttributes
  { amount :: Int,
    status :: T.Text,
    partialApprovalAllowed :: Int,
    merchant :: Merchant,
    recurring :: Bool,
    createdAt :: DateTime,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data AuthorizationRequestApprovedAttributes = AuthorizationRequestApprovedAttributes
  { amount :: Int,
    approvedAmount :: Int,
    status :: T.Text,
    partialApprovalAllowed :: Int,
    merchant :: Merchant,
    recurring :: Bool,
    createdAt :: DateTime,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data AuthorizationRequestApproved = AuthorizationRequestApproved
  { id :: T.Text,
    attributes :: AuthorizationRequestApprovedAttributes,
    relationships :: AuthorizationRequestRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data AuthorizationRequestPendingAttributes = AuthorizationRequestPendingAttributes
  { amount :: Int,
    status :: T.Text,
    partialApprovalAllowed :: Int,
    merchant :: Merchant,
    recurring :: Bool,
    available :: Int,
    ecommerce :: Bool,
    cardPresent :: Maybe Bool,
    healthcareAmounts :: Maybe HealthcareAmounts,
    createdAt :: DateTime,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data AuthorizationRequestPending = AuthorizationRequestPending
  { id :: T.Text,
    attributes :: AuthorizationRequestPendingAttributes,
    relationships :: AuthorizationRequestRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data AuthorizationRequestDeclinedAttributes = AuthorizationRequestDeclinedAttributes
  { amount :: Int,
    status :: T.Text,
    partialApprovalAllowed :: Int,
    merchant :: Merchant,
    recurring :: Bool,
    declineReason :: T.Text,
    createdAt :: DateTime,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data AuthorizationRequestDeclined = AuthorizationRequestDeclined
  { id :: T.Text,
    attributes :: AuthorizationRequestDeclinedAttributes,
    relationships :: AuthorizationRequestRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

-- * BulkPayments

data BulkPaymentsFailedAttributes = BulkPaymentsFailedAttributes
  { index :: T.Text,
    error :: T.Text,
    idempotencyKey :: Maybe T.Text,
    createdAt :: DateTime,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

newtype BulkPaymentsRelationships = BulkPaymentsRelationships
  { bulkPayments :: BulkPaymentsId
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data BulkPaymentsFailed = BulkPaymentsFailed
  { id :: T.Text,
    attributes :: BulkPaymentsFailedAttributes,
    relationships :: BulkPaymentsRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data BulkPaymentsFinished = BulkPaymentsFinished
  { id :: T.Text,
    attributes :: BaseEventAttributes,
    relationships :: BulkPaymentsRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

-- * Card

data CardRelationships = CardRelationships
  { account :: AccountId,
    customer :: CustomerId,
    card :: CardId
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data CardCreated = CardCreated
  { id :: T.Text,
    attributes :: BaseEventAttributes,
    relationships :: CardRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data CardActivated = CardActivated
  { id :: T.Text,
    attributes :: BaseEventAttributes,
    relationships :: CardRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data CardStatusChangedAttributes = CardStatusChangedAttributes
  { createdAt :: DateTime,
    newStatus :: T.Text,
    previousStatus :: T.Text,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data CardStatusChanged = CardStatusChanged
  { id :: T.Text,
    attributes :: CardStatusChangedAttributes,
    relationships :: CardRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

-- * Customer

data CustomerCreatedRelationships = CustomerCreatedRelationships
  { customer :: CustomerId,
    application :: ApplicationId
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

newtype CustomerRelationships = CustomerRelationships
  { customer :: CustomerId
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data CustomerCreated = CustomerCreated
  { id :: T.Text,
    attributes :: BaseEventAttributes,
    relationships :: CustomerCreatedRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data CustomerUpdated = CustomerUpdated
  { id :: T.Text,
    attributes :: BaseEventAttributes,
    relationships :: CustomerRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data CustomerArchived = CustomerArchived
  { id :: T.Text,
    attributes :: BaseEventAttributes,
    relationships :: CustomerRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

-- * Document

data DocumentRelationships = DocumentRelationships
  { document :: DocumentId,
    application :: ApplicationId
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data DocumentApproved = DocumentApproved
  { id :: T.Text,
    attributes :: BaseEventAttributes,
    relationships :: DocumentRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data DocumentRejectedAttributes = DocumentRejectedAttributes
  { reason :: T.Text,
    reasonCode :: T.Text,
    createdAt :: DateTime,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data DocumentRejected = DocumentRejected
  { id :: T.Text,
    attributes :: DocumentRejectedAttributes,
    relationships :: DocumentRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

-- * Payment

data PaymentRelationships = PaymentRelationships
  { account :: AccountId,
    customer :: CustomerId,
    payment :: PaymentId
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data PaymentAttributes = PaymentAttributes
  { createdAt :: DateTime,
    direction :: Direction,
    amount :: Int,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data PaymentCreatedAttributes = PaymentCreatedAttributes
  { createdAt :: DateTime,
    direction :: Direction,
    status :: PaymentStatus,
    amount :: Int,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data PaymentCreated = PaymentCreated
  { id :: T.Text,
    attributes :: PaymentCreatedAttributes,
    relationships :: PaymentRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data PaymentUpdateAttributes = PaymentUpdateAttributes
  { createdAt :: DateTime,
    direction :: Direction,
    previousStatus :: PaymentStatus,
    amount :: Int,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data PaymentClearing = PaymentClearing
  { id :: T.Text,
    attributes :: PaymentUpdateAttributes,
    relationships :: PaymentRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data PaymentSent = PaymentSent
  { id :: T.Text,
    attributes :: PaymentUpdateAttributes,
    relationships :: PaymentRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data PaymentReturned = PaymentReturned
  { id :: T.Text,
    attributes :: PaymentUpdateAttributes,
    relationships :: PaymentRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data PaymentRejectedAttributes = PaymentRejectedAttributes
  { createdAt :: DateTime,
    direction :: Direction,
    previousStatus :: PaymentStatus,
    amount :: Int,
    reason :: T.Text,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data PaymentRejected = PaymentRejected
  { id :: T.Text,
    attributes :: PaymentRejectedAttributes,
    relationships :: PaymentRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data PaymentCanceled = PaymentCanceled
  { id :: T.Text,
    attributes :: PaymentAttributes,
    relationships :: PaymentRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data PaymentPendingReview = PaymentPendingReview
  { id :: T.Text,
    attributes :: PaymentAttributes,
    relationships :: PaymentRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

-- * RecurringPayment

data RecurringPaymentAttributes = RecurringPaymentAttributes
  { createdAt :: DateTime,
    amount :: Int,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data RecurringPaymentRelationships = RecurringPaymentRelationships
  { account :: AccountId,
    customer :: CustomerId,
    recurringPayment :: RecurringPaymentId
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data RecurringPaymentCreatedAttributes = RecurringPaymentCreatedAttributes
  { createdAt :: DateTime,
    amount :: Int,
    status :: RecurringPaymentStatus,
    nextScheduledAction :: Date,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data RecurringPaymentCreated = RecurringPaymentCreated
  { id :: T.Text,
    attributes :: RecurringPaymentCreatedAttributes,
    relationships :: RecurringPaymentRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data RecurringPaymentStatusChangedAttributes = RecurringPaymentStatusChangedAttributes
  { createdAt :: DateTime,
    amount :: Int,
    status :: RecurringPaymentStatus,
    previousStatus :: RecurringPaymentStatus,
    numberOfPayments :: Int,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data RecurringPaymentStatusChanged = RecurringPaymentStatusChanged
  { id :: T.Text,
    attributes :: RecurringPaymentStatusChangedAttributes,
    relationships :: RecurringPaymentRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data RecurringPaymentFailedAttributes = RecurringPaymentFailedAttributes
  { createdAt :: DateTime,
    amount :: Int,
    error :: T.Text,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data RecurringPaymentFailed = RecurringPaymentFailed
  { id :: T.Text,
    attributes :: RecurringPaymentFailedAttributes,
    relationships :: RecurringPaymentRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

-- * Statements

data StatementsAttributes = StatementsAttributes
  { createdAt :: DateTime,
    period :: T.Text, --  "2021-02"
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data StatementsCreated = StatementsCreated
  { id :: T.Text,
    attributes :: StatementsAttributes,
    relationships :: BaseEventRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

-- * Transaction

data TransactionAttributes = TransactionAttributes
  { createdAt :: DateTime,
    summary :: T.Text,
    direction :: Direction,
    amount :: Int,
    balance :: Int,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data TransactionRelationships = TransactionRelationships
  { account :: AccountId,
    customer :: CustomerId,
    transaction :: RelationshipsObject, -- "receivedAchTransaction" | "purchase" | etc
    payment :: PaymentId
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data TransactionCreated = TransactionCreated
  { id :: T.Text,
    attributes :: TransactionAttributes,
    relationships :: TransactionRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data TransactionUpdatedAttributes = TransactionUpdatedAttributes
  { createdAt :: DateTime,
    interchange :: Int,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

newtype TransactionUpdatedRelationships = TransactionUpdatedRelationships {transaction :: RelationshipsObject}
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

-- | Occurs when a PurchaseTransaction, AtmTransaction, or CardTransaction is updated with the interchange amount, calculated at the end of each day.
data TransactionUpdated = TransactionUpdated
  { id :: T.Text,
    attributes :: TransactionUpdatedAttributes,
    relationships :: TransactionUpdatedRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

-- * ReceivedPayment

data ReceivedPaymentAttributes = ReceivedPaymentAttributes
  { createdAt :: DateTime,
    status :: ReceivedPaymentStatus,
    id :: T.Text,
    -- type :: T.Text,
    amount :: Int,
    completionDate :: Date,
    wasAdvanced :: Maybe Bool,
    companyName :: T.Text,
    counterpartyRoutingNumber :: T.Text,
    traceNumber :: T.Text,
    description :: T.Text,
    secCode :: Maybe T.Text,
    addenda :: Maybe T.Text,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data ReceivedPaymentRelationships = ReceivedPaymentRelationships
  { account :: AccountId,
    customer :: CustomerId,
    receivedPayment :: ReceivedPaymentId
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data ReceivedPaymentCreated = ReceivedPaymentCreated
  { id :: T.Text,
    attributes :: ReceivedPaymentAttributes,
    relationships :: ReceivedPaymentRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data ReceivedPaymentUpdatedAttributes = ReceivedPaymentUpdatedAttributes
  { createdAt :: DateTime,
    previousStatus :: ReceivedPaymentStatus,
    wasAdvanced :: Bool,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data ReceivedPaymentAdvanced = ReceivedPaymentAdvanced
  { id :: T.Text,
    attributes :: ReceivedPaymentUpdatedAttributes,
    relationships :: ReceivedPaymentRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data ReceivedPaymentCompleted = ReceivedPaymentCompleted
  { id :: T.Text,
    attributes :: ReceivedPaymentUpdatedAttributes,
    relationships :: ReceivedPaymentRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data ReceivedPaymentReturned = ReceivedPaymentReturned
  { id :: T.Text,
    attributes :: ReceivedPaymentUpdatedAttributes,
    relationships :: ReceivedPaymentRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

-- * ChargeBack

data ChargeBackCreatedAttributes = ChargeBackCreatedAttributes
  { createdAt :: DateTime,
    amount :: Int,
    description :: T.Text,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data ChargeBackCreatedRelationships = ChargeBackCreatedRelationships
  { account :: AccountId,
    customer :: CustomerId,
    chargeback :: RelationshipsObject,
    counterpartyAccount :: AccountId,
    transaction :: TransactionId
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data ChargeBackCreated = ChargeBackCreated
  { id :: T.Text,
    attributes :: ChargeBackCreatedAttributes,
    relationships :: ChargeBackCreatedRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

-- * Rewards

data RewardRelationship = RewardRelationship
  { reward :: RelationshipsObject,
    fundingAccount :: AccountId,
    receivingAccount :: AccountId,
    customer :: CustomerId
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data RewardSent = RewardSent
  { id :: T.Text,
    attributes :: BaseEventAttributes,
    relationships :: RewardRelationship
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data RewardRejected = RewardRejected
  { id :: T.Text,
    attributes :: BaseEventAttributes,
    relationships :: RewardRelationship
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

-- * CheckDeposit

data CheckDepositRelationships = CheckDepositRelationships
  { account :: AccountId,
    customer :: CustomerId,
    checkDeposit :: RelationshipsObject
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data CheckDepositCreatedAttributes = CheckDepositCreatedAttributes
  { createdAt :: DateTime,
    status :: T.Text, -- TODO @asimuskov: add enum CheckDepositStatus
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data CheckDepositCreated = CheckDepositCreated
  { id :: T.Text,
    attributes :: CheckDepositCreatedAttributes,
    relationships :: CheckDepositRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data CheckDepositUpdateAttributes = CheckDepositUpdatesAttributes
  { createdAt :: DateTime,
    previousStatus :: T.Text, -- TODO @asimuskov: add enum CheckDepositStatus
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data CheckDepositPendingReview = CheckDepositPendingReview
  { id :: T.Text,
    attributes :: CheckDepositUpdateAttributes,
    relationships :: CheckDepositRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data CheckDepositPending = CheckDepositPending
  { id :: T.Text,
    attributes :: CheckDepositUpdateAttributes,
    relationships :: CheckDepositRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data CheckDepositRejectedAttributes = CheckDepositRejectedAttributes
  { createdAt :: DateTime,
    previousStatus :: T.Text, -- TODO @asimuskov: add enum CheckDepositStatus
    reason :: T.Text,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data CheckDepositRejected = CheckDepositRejected
  { id :: T.Text,
    attributes :: CheckDepositRejectedAttributes,
    relationships :: CheckDepositRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data CheckDepositClearing = CheckDepositClearing
  { id :: T.Text,
    attributes :: CheckDepositUpdateAttributes,
    relationships :: CheckDepositRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data CheckDepositSent = CheckDepositSent
  { id :: T.Text,
    attributes :: CheckDepositUpdateAttributes,
    relationships :: CheckDepositRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data CheckDepositReturned = CheckDepositReturned
  { id :: T.Text,
    attributes :: CheckDepositUpdateAttributes,
    relationships :: CheckDepositRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

-- * DeclinedIncoming

data DeclinedIncomingPaymentAttributes = DeclinedIncomingPaymentAttributes
  { createdAt :: DateTime,
    amount :: Int,
    direction :: Direction,
    reason :: T.Text,
    paymentType :: T.Text,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data DeclinedIncomingPayment = DeclinedIncomingPayment
  { id :: T.Text,
    attributes :: DeclinedIncomingPaymentAttributes,
    relationships :: BaseEventRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

-- * Dispute

data DisputeAttributes = DisputeAttributes
  { createdAt :: DateTime,
    amount :: Int,
    description :: T.Text,
    source :: T.Text,
    status :: T.Text,
    tags :: Maybe Tags
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data DisputeRelationships = DisputeRelationships
  { account :: AccountId,
    customer :: CustomerId,
    dispute :: RelationshipsObject,
    transaction :: TransactionId
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data DisputeCreated = DisputeCreated
  { id :: T.Text,
    attributes :: DisputeAttributes,
    relationships :: DisputeRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)

data DisputeStatusChanged = DisputeStatusChanged
  { id :: T.Text,
    attributes :: DisputeAttributes,
    relationships :: DisputeRelationships
  }
  deriving (Show, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving anyclass (J.FromJSON, J.ToJSON)
