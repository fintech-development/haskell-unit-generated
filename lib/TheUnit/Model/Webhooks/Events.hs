-- NOTE:
-- All types presented here are written with hands
-- Therefore, here may contain errors and inconsistencies with real API
-- Moreover, the description of the API in DOC or Typescript module
-- may differ from the actual API in answers
-- [Webhooks Events](https://docs.unit.co/events)
-- [TypeScript](https://github.com/unit-finance/unit-node-sdk/blob/main/types/events.ts)
{-# LANGUAGE StrictData #-}

module TheUnit.Model.Webhooks.Events where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as J
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import GHC.Generics (Generic)
import TheUnit.Model.Core (mergeAesonObjects)
import TheUnit.Model.Orphans ()
import TheUnit.Model.Webhooks.WebhookPayload
import Prelude hiding (error, id)

data UnitEvent
  = UnitEvent'AccountCreated
  | UnitEvent'AccountClosed
  | UnitEvent'AccountFrozen
  | UnitEvent'AccountReopened
  | UnitEvent'AccountUnfrozen
  | UnitEvent'ApplicationCanceled
  | UnitEvent'ApplicationDenied
  | UnitEvent'ApplicationAwaitingDocuments
  | UnitEvent'ApplicationPendingReview
  | UnitEvent'AuthorizationCreated
  | UnitEvent'AuthorizationCanceled
  | UnitEvent'AuthorizationDeclined
  | UnitEvent'AuthorizationAmountChanged
  | UnitEvent'AuthorizationRequestApproved
  | UnitEvent'AuthorizationRequestPending
  | UnitEvent'AuthorizationRequestDeclined
  | UnitEvent'BulkPaymentsFailed
  | UnitEvent'BulkPaymentsFinished
  | UnitEvent'CardCreated
  | UnitEvent'CardActivated
  | UnitEvent'CardStatusChanged
  | UnitEvent'CustomerCreated
  | UnitEvent'CustomerUpdated
  | UnitEvent'CustomerArchived
  | UnitEvent'DocumentApproved
  | UnitEvent'DocumentRejected
  | UnitEvent'PaymentCreated
  | UnitEvent'PaymentClearing
  | UnitEvent'PaymentSent
  | UnitEvent'PaymentReturned
  | UnitEvent'PaymentRejected
  | UnitEvent'PaymentCanceled
  | UnitEvent'PaymentPendingReview
  | UnitEvent'RecurringPaymentCreated
  | UnitEvent'RecurringPaymentStatusChanged
  | UnitEvent'RecurringPaymentFailed
  | UnitEvent'StatementsCreated
  | UnitEvent'TransactionCreated
  | UnitEvent'TransactionUpdated
  | UnitEvent'ReceivedPaymentCreated
  | UnitEvent'ReceivedPaymentAdvanced
  | UnitEvent'ReceivedPaymentCompleted
  | UnitEvent'ReceivedPaymentReturned
  | UnitEvent'ChargebackCreated
  | UnitEvent'RewardSent
  | UnitEvent'RewardRejected
  | UnitEvent'CheckDepositCreated
  | UnitEvent'CheckDepositPendingReview
  | UnitEvent'CheckDepositPending
  | UnitEvent'CheckDepositRejected
  | UnitEvent'CheckDepositClearing
  | UnitEvent'CheckDepositSent
  | UnitEvent'CheckDepositReturned
  | UnitEvent'DeclinedIncomingPaymentCreated
  | UnitEvent'DisputeCreated
  | UnitEvent'DisputeStatusChanged
  | UnitEvent'Unknown T.Text
  deriving (Show, Eq, Generic)

instance J.FromJSON UnitEvent where
  parseJSON o =
    toE'UnitEventType <$> J.parseJSON o

instance J.ToJSON UnitEvent where
  toJSON = J.toJSON . fromE'UnitEventType

data UnitWebhook
  = -- Accounts
    UnitWebhook'AccountCreated AccountCreated
  | UnitWebhook'AccountClosed AccountClosed
  | UnitWebhook'AccountFrozen AccountFrozen
  | UnitWebhook'AccountReopened AccountReopened
  | UnitWebhook'AccountUnfrozen AccountUnfrozen
  | -- Application
    UnitWebhook'ApplicationCanceled ApplicationCanceled
  | UnitWebhook'ApplicationDenied ApplicationDenied
  | UnitWebhook'ApplicationAwaitingDocuments ApplicationAwaitingDocuments
  | UnitWebhook'ApplicationPendingReview ApplicationPendingReview
  | -- Customer
    UnitWebhook'CustomerCreated CustomerCreated
  | UnitWebhook'CustomerUpdated CustomerUpdated
  | UnitWebhook'CustomerArchived CustomerArchived
  | -- Document
    UnitWebhook'DocumentApproved DocumentApproved
  | UnitWebhook'DocumentRejected DocumentRejected
  | -- Payment
    UnitWebhook'PaymentCreated PaymentCreated
  | UnitWebhook'PaymentClearing PaymentClearing
  | UnitWebhook'PaymentSent PaymentSent
  | UnitWebhook'PaymentReturned PaymentReturned
  | UnitWebhook'PaymentRejected PaymentRejected
  | UnitWebhook'PaymentCanceled PaymentCanceled
  | UnitWebhook'PaymentPendingReview PaymentPendingReview
  | -- ReceivedPayment
    UnitWebhook'ReceivedPaymentCreated ReceivedPaymentCreated
  | UnitWebhook'ReceivedPaymentAdvanced ReceivedPaymentAdvanced
  | UnitWebhook'ReceivedPaymentCompleted ReceivedPaymentCompleted
  | UnitWebhook'ReceivedPaymentReturned ReceivedPaymentReturned
  | -- RecurringPayment
    UnitWebhook'RecurringPaymentCreated RecurringPaymentCreated
  | UnitWebhook'RecurringPaymentStatusChanged RecurringPaymentStatusChanged
  | UnitWebhook'RecurringPaymentFailed RecurringPaymentFailed
  | -- Statements
    UnitWebhook'StatementsCreated StatementsCreated
  | -- Transaction
    UnitWebhook'TransactionCreated TransactionCreated
  | UnitWebhook'TransactionUpdated TransactionUpdated
  | -- | Unhandled webhook:
    -- - One of events which was not added in Enum
    -- - New Events in Unit which not described here
    -- - Valid Event in Enum, but was not parsed correctly
    UnitUnhandledWebhook UnitEvent J.Value
  deriving (Show, Generic)

instance J.FromJSON UnitWebhook where
  parseJSON val = flip (J.withObject "UnitWebhook") val \o ->
    UnitEvent'AccountCreated --> UnitWebhook'AccountCreated
      <|> UnitEvent'AccountClosed --> UnitWebhook'AccountClosed
      <|> UnitEvent'AccountFrozen --> UnitWebhook'AccountFrozen
      <|> UnitEvent'AccountReopened --> UnitWebhook'AccountReopened
      <|> UnitEvent'AccountUnfrozen --> UnitWebhook'AccountUnfrozen
      <|> UnitEvent'ApplicationCanceled --> UnitWebhook'ApplicationCanceled
      <|> UnitEvent'ApplicationDenied --> UnitWebhook'ApplicationDenied
      <|> UnitEvent'ApplicationAwaitingDocuments --> UnitWebhook'ApplicationAwaitingDocuments
      <|> UnitEvent'ApplicationPendingReview --> UnitWebhook'ApplicationPendingReview
      <|> UnitEvent'CustomerCreated --> UnitWebhook'CustomerCreated
      <|> UnitEvent'CustomerUpdated --> UnitWebhook'CustomerUpdated
      <|> UnitEvent'CustomerArchived --> UnitWebhook'CustomerArchived
      <|> UnitEvent'DocumentApproved --> UnitWebhook'DocumentApproved
      <|> UnitEvent'DocumentRejected --> UnitWebhook'DocumentRejected
      <|> UnitEvent'PaymentCreated --> UnitWebhook'PaymentCreated
      <|> UnitEvent'PaymentClearing --> UnitWebhook'PaymentClearing
      <|> UnitEvent'PaymentSent --> UnitWebhook'PaymentSent
      <|> UnitEvent'PaymentReturned --> UnitWebhook'PaymentReturned
      <|> UnitEvent'PaymentRejected --> UnitWebhook'PaymentRejected
      <|> UnitEvent'PaymentCanceled --> UnitWebhook'PaymentCanceled
      <|> UnitEvent'PaymentPendingReview --> UnitWebhook'PaymentPendingReview
      <|> UnitEvent'ReceivedPaymentCreated --> UnitWebhook'ReceivedPaymentCreated
      <|> UnitEvent'ReceivedPaymentAdvanced --> UnitWebhook'ReceivedPaymentAdvanced
      <|> UnitEvent'ReceivedPaymentCompleted --> UnitWebhook'ReceivedPaymentCompleted
      <|> UnitEvent'ReceivedPaymentReturned --> UnitWebhook'ReceivedPaymentReturned
      <|> UnitEvent'RecurringPaymentCreated --> UnitWebhook'RecurringPaymentCreated
      <|> UnitEvent'RecurringPaymentStatusChanged --> UnitWebhook'RecurringPaymentStatusChanged
      <|> UnitEvent'RecurringPaymentFailed --> UnitWebhook'RecurringPaymentFailed
      <|> UnitEvent'StatementsCreated --> UnitWebhook'StatementsCreated
      <|> UnitEvent'TransactionCreated --> UnitWebhook'TransactionCreated
      <|> UnitEvent'TransactionUpdated --> UnitWebhook'TransactionUpdated
      <|> ( do
              event@(UnitEvent'Unknown _) <- o .: "type"
              UnitUnhandledWebhook event <$> J.parseJSON val
          )
      <|> ( do
              event <- o .: "type"
              UnitUnhandledWebhook event <$> J.parseJSON val
          )
    where
      (-->) :: J.FromJSON a => UnitEvent -> (a -> UnitWebhook) -> Parser UnitWebhook
      (-->) type' constructor = do
        flip (J.withObject "UnitWebhook") val \o ->
          withType o type' constructor
      withType :: J.FromJSON a => J.Object -> UnitEvent -> (a -> UnitWebhook) -> Parser UnitWebhook
      withType o type' constructor = do
        type_ <- o .: "type"
        guard (type_ == type')
        constructor <$> J.parseJSON val

instance J.ToJSON UnitWebhook where
  toJSON wh =
    let withType type' payload =
          mergeAesonObjects [J.object ["type" .= type'], J.toJSON payload]
     in case wh of
          UnitWebhook'AccountCreated payload -> withType UnitEvent'AccountCreated payload
          UnitWebhook'AccountClosed payload -> withType UnitEvent'AccountClosed payload
          UnitWebhook'AccountFrozen payload -> withType UnitEvent'AccountFrozen payload
          UnitWebhook'AccountReopened payload -> withType UnitEvent'AccountReopened payload
          UnitWebhook'AccountUnfrozen payload -> withType UnitEvent'AccountUnfrozen payload
          UnitWebhook'ApplicationCanceled payload -> withType UnitEvent'ApplicationCanceled payload
          UnitWebhook'ApplicationDenied payload -> withType UnitEvent'ApplicationDenied payload
          UnitWebhook'ApplicationAwaitingDocuments payload -> withType UnitEvent'ApplicationAwaitingDocuments payload
          UnitWebhook'ApplicationPendingReview payload -> withType UnitEvent'ApplicationPendingReview payload
          UnitWebhook'CustomerCreated payload -> withType UnitEvent'CustomerCreated payload
          UnitWebhook'CustomerUpdated payload -> withType UnitEvent'CustomerUpdated payload
          UnitWebhook'CustomerArchived payload -> withType UnitEvent'CustomerArchived payload
          UnitWebhook'DocumentApproved payload -> withType UnitEvent'DocumentApproved payload
          UnitWebhook'DocumentRejected payload -> withType UnitEvent'DocumentRejected payload
          UnitWebhook'PaymentCreated payload -> withType UnitEvent'PaymentCreated payload
          UnitWebhook'PaymentClearing payload -> withType UnitEvent'PaymentClearing payload
          UnitWebhook'PaymentSent payload -> withType UnitEvent'PaymentSent payload
          UnitWebhook'PaymentReturned payload -> withType UnitEvent'PaymentReturned payload
          UnitWebhook'PaymentRejected payload -> withType UnitEvent'PaymentRejected payload
          UnitWebhook'PaymentCanceled payload -> withType UnitEvent'PaymentCanceled payload
          UnitWebhook'PaymentPendingReview payload -> withType UnitEvent'PaymentPendingReview payload
          UnitWebhook'ReceivedPaymentCreated payload -> withType UnitEvent'ReceivedPaymentCreated payload
          UnitWebhook'ReceivedPaymentAdvanced payload -> withType UnitEvent'ReceivedPaymentAdvanced payload
          UnitWebhook'ReceivedPaymentCompleted payload -> withType UnitEvent'ReceivedPaymentCompleted payload
          UnitWebhook'ReceivedPaymentReturned payload -> withType UnitEvent'ReceivedPaymentReturned payload
          UnitWebhook'RecurringPaymentCreated payload -> withType UnitEvent'RecurringPaymentCreated payload
          UnitWebhook'RecurringPaymentStatusChanged payload -> withType UnitEvent'RecurringPaymentStatusChanged payload
          UnitWebhook'RecurringPaymentFailed payload -> withType UnitEvent'RecurringPaymentFailed payload
          UnitWebhook'StatementsCreated payload -> withType UnitEvent'StatementsCreated payload
          UnitWebhook'TransactionCreated payload -> withType UnitEvent'TransactionCreated payload
          UnitWebhook'TransactionUpdated payload -> withType UnitEvent'TransactionUpdated payload
          UnitUnhandledWebhook event payload -> withType event payload

-- | unwrap 'UnitEvent' enum
fromE'UnitEventType :: UnitEvent -> T.Text
fromE'UnitEventType = \case
  UnitEvent'AccountCreated -> "account.created"
  UnitEvent'AccountClosed -> "account.closed"
  UnitEvent'AccountFrozen -> "account.frozen"
  UnitEvent'AccountReopened -> "account.reopened"
  UnitEvent'AccountUnfrozen -> "account.unfrozen"
  UnitEvent'ApplicationCanceled -> "application.canceled"
  UnitEvent'ApplicationDenied -> "application.denied"
  UnitEvent'ApplicationAwaitingDocuments -> "application.awaitingDocuments"
  UnitEvent'ApplicationPendingReview -> "application.pendingReview"
  UnitEvent'AuthorizationCreated -> "authorization.created"
  UnitEvent'AuthorizationCanceled -> "authorization.canceled"
  UnitEvent'AuthorizationDeclined -> "authorization.declined"
  UnitEvent'AuthorizationAmountChanged -> "authorization.amountChanged"
  UnitEvent'AuthorizationRequestApproved -> "authorizationRequest.approved"
  UnitEvent'AuthorizationRequestPending -> "authorizationRequest.pending"
  UnitEvent'AuthorizationRequestDeclined -> "authorizationRequest.declined"
  UnitEvent'BulkPaymentsFailed -> "bulkPayments.failed"
  UnitEvent'BulkPaymentsFinished -> "bulkPayments.finished"
  UnitEvent'CardCreated -> "card.created"
  UnitEvent'CardActivated -> "card.activated"
  UnitEvent'CardStatusChanged -> "card.statusChanged"
  UnitEvent'CustomerCreated -> "customer.created"
  UnitEvent'CustomerUpdated -> "customer.updated"
  UnitEvent'CustomerArchived -> "customer.archived"
  UnitEvent'DocumentApproved -> "document.approved"
  UnitEvent'DocumentRejected -> "document.rejected"
  UnitEvent'PaymentCreated -> "payment.created"
  UnitEvent'PaymentClearing -> "payment.clearing"
  UnitEvent'PaymentSent -> "payment.sent"
  UnitEvent'PaymentReturned -> "payment.returned"
  UnitEvent'PaymentRejected -> "payment.rejected"
  UnitEvent'PaymentCanceled -> "payment.canceled"
  UnitEvent'PaymentPendingReview -> "payment.pendingReview"
  UnitEvent'RecurringPaymentCreated -> "recurringPayment.created"
  UnitEvent'RecurringPaymentStatusChanged -> "recurringPayment.statusChanged"
  UnitEvent'RecurringPaymentFailed -> "recurringPayment.failed"
  UnitEvent'StatementsCreated -> "statements.created"
  UnitEvent'TransactionCreated -> "transaction.created"
  UnitEvent'TransactionUpdated -> "transaction.updated"
  UnitEvent'ReceivedPaymentCreated -> "receivedPayment.created"
  UnitEvent'ReceivedPaymentAdvanced -> "receivedPayment.advanced"
  UnitEvent'ReceivedPaymentCompleted -> "receivedPayment.completed"
  UnitEvent'ReceivedPaymentReturned -> "receivedPayment.returned"
  UnitEvent'ChargebackCreated -> "chargeback.created"
  UnitEvent'RewardSent -> "reward.sent"
  UnitEvent'RewardRejected -> "reward.rejected"
  UnitEvent'CheckDepositCreated -> "checkDeposit.created"
  UnitEvent'CheckDepositPendingReview -> "checkDeposit.pendingReview"
  UnitEvent'CheckDepositPending -> "checkDeposit.pending"
  UnitEvent'CheckDepositRejected -> "checkDeposit.rejected"
  UnitEvent'CheckDepositClearing -> "checkDeposit.clearing"
  UnitEvent'CheckDepositSent -> "checkDeposit.sent"
  UnitEvent'CheckDepositReturned -> "checkDeposit.returned"
  UnitEvent'DeclinedIncomingPaymentCreated -> "declinedIncomingPayment.created"
  UnitEvent'DisputeCreated -> "dispute.created"
  UnitEvent'DisputeStatusChanged -> "dispute.statusChanged"
  UnitEvent'Unknown str -> str

-- | parse 'UnitEvent' enum
toE'UnitEventType :: T.Text -> UnitEvent
toE'UnitEventType = \case
  "account.created" -> UnitEvent'AccountCreated
  "account.closed" -> UnitEvent'AccountClosed
  "account.frozen" -> UnitEvent'AccountFrozen
  "account.reopened" -> UnitEvent'AccountReopened
  "account.unfrozen" -> UnitEvent'AccountUnfrozen
  "application.canceled" -> UnitEvent'ApplicationCanceled
  "application.denied" -> UnitEvent'ApplicationDenied
  "application.awaitingDocuments" -> UnitEvent'ApplicationAwaitingDocuments
  "application.pendingReview" -> UnitEvent'ApplicationPendingReview
  "authorization.created" -> UnitEvent'AuthorizationCreated
  "authorization.canceled" -> UnitEvent'AuthorizationCanceled
  "authorization.declined" -> UnitEvent'AuthorizationDeclined
  "authorization.amountChanged" -> UnitEvent'AuthorizationAmountChanged
  "authorizationRequest.approved" -> UnitEvent'AuthorizationRequestApproved
  "authorizationRequest.pending" -> UnitEvent'AuthorizationRequestPending
  "authorizationRequest.declined" -> UnitEvent'AuthorizationRequestDeclined
  "bulkPayments.failed" -> UnitEvent'BulkPaymentsFailed
  "bulkPayments.finished" -> UnitEvent'BulkPaymentsFinished
  "card.created" -> UnitEvent'CardCreated
  "card.activated" -> UnitEvent'CardActivated
  "card.statusChanged" -> UnitEvent'CardStatusChanged
  "customer.created" -> UnitEvent'CustomerCreated
  "customer.updated" -> UnitEvent'CustomerUpdated
  "customer.archived" -> UnitEvent'CustomerArchived
  "document.approved" -> UnitEvent'DocumentApproved
  "document.rejected" -> UnitEvent'DocumentRejected
  "payment.created" -> UnitEvent'PaymentCreated
  "payment.clearing" -> UnitEvent'PaymentClearing
  "payment.sent" -> UnitEvent'PaymentSent
  "payment.returned" -> UnitEvent'PaymentReturned
  "payment.rejected" -> UnitEvent'PaymentRejected
  "payment.canceled" -> UnitEvent'PaymentCanceled
  "payment.pendingReview" -> UnitEvent'PaymentPendingReview
  "recurringPayment.created" -> UnitEvent'RecurringPaymentCreated
  "recurringPayment.statusChanged" -> UnitEvent'RecurringPaymentStatusChanged
  "recurringPayment.failed" -> UnitEvent'RecurringPaymentFailed
  "statements.created" -> UnitEvent'StatementsCreated
  "transaction.created" -> UnitEvent'TransactionCreated
  "transaction.updated" -> UnitEvent'TransactionUpdated
  "receivedPayment.created" -> UnitEvent'ReceivedPaymentCreated
  "receivedPayment.advanced" -> UnitEvent'ReceivedPaymentAdvanced
  "receivedPayment.completed" -> UnitEvent'ReceivedPaymentCompleted
  "receivedPayment.returned" -> UnitEvent'ReceivedPaymentReturned
  "chargeback.created" -> UnitEvent'ChargebackCreated
  "reward.sent" -> UnitEvent'RewardSent
  "reward.rejected" -> UnitEvent'RewardRejected
  "checkDeposit.created" -> UnitEvent'CheckDepositCreated
  "checkDeposit.pendingReview" -> UnitEvent'CheckDepositPendingReview
  "checkDeposit.pending" -> UnitEvent'CheckDepositPending
  "checkDeposit.rejected" -> UnitEvent'CheckDepositRejected
  "checkDeposit.clearing" -> UnitEvent'CheckDepositClearing
  "checkDeposit.sent" -> UnitEvent'CheckDepositSent
  "checkDeposit.returned" -> UnitEvent'CheckDepositReturned
  "declinedIncomingPayment.created" -> UnitEvent'DeclinedIncomingPaymentCreated
  "dispute.created" -> UnitEvent'DisputeCreated
  "dispute.statusChanged" -> UnitEvent'DisputeStatusChanged
  str -> UnitEvent'Unknown str

webhookEvent :: UnitWebhook -> UnitEvent
webhookEvent = \case
  UnitWebhook'AccountCreated _ -> UnitEvent'AccountCreated
  UnitWebhook'AccountClosed _ -> UnitEvent'AccountClosed
  UnitWebhook'AccountFrozen _ -> UnitEvent'AccountFrozen
  UnitWebhook'AccountReopened _ -> UnitEvent'AccountReopened
  UnitWebhook'AccountUnfrozen _ -> UnitEvent'AccountUnfrozen
  UnitWebhook'ApplicationCanceled _ -> UnitEvent'ApplicationCanceled
  UnitWebhook'ApplicationDenied _ -> UnitEvent'ApplicationDenied
  UnitWebhook'ApplicationAwaitingDocuments _ -> UnitEvent'ApplicationAwaitingDocuments
  UnitWebhook'ApplicationPendingReview _ -> UnitEvent'ApplicationPendingReview
  UnitWebhook'CustomerCreated _ -> UnitEvent'CustomerCreated
  UnitWebhook'CustomerUpdated _ -> UnitEvent'CustomerUpdated
  UnitWebhook'CustomerArchived _ -> UnitEvent'CustomerArchived
  UnitWebhook'DocumentApproved _ -> UnitEvent'DocumentApproved
  UnitWebhook'DocumentRejected _ -> UnitEvent'DocumentRejected
  UnitWebhook'PaymentCreated _ -> UnitEvent'PaymentCreated
  UnitWebhook'PaymentClearing _ -> UnitEvent'PaymentClearing
  UnitWebhook'PaymentSent _ -> UnitEvent'PaymentSent
  UnitWebhook'PaymentReturned _ -> UnitEvent'PaymentReturned
  UnitWebhook'PaymentRejected _ -> UnitEvent'PaymentRejected
  UnitWebhook'PaymentCanceled _ -> UnitEvent'PaymentCanceled
  UnitWebhook'PaymentPendingReview _ -> UnitEvent'PaymentPendingReview
  UnitWebhook'ReceivedPaymentCreated _ -> UnitEvent'ReceivedPaymentCreated
  UnitWebhook'ReceivedPaymentAdvanced _ -> UnitEvent'ReceivedPaymentAdvanced
  UnitWebhook'ReceivedPaymentCompleted _ -> UnitEvent'ReceivedPaymentCompleted
  UnitWebhook'ReceivedPaymentReturned _ -> UnitEvent'ReceivedPaymentReturned
  UnitWebhook'RecurringPaymentCreated _ -> UnitEvent'RecurringPaymentCreated
  UnitWebhook'RecurringPaymentStatusChanged _ -> UnitEvent'RecurringPaymentStatusChanged
  UnitWebhook'RecurringPaymentFailed _ -> UnitEvent'RecurringPaymentFailed
  UnitWebhook'StatementsCreated _ -> UnitEvent'StatementsCreated
  UnitWebhook'TransactionCreated _ -> UnitEvent'TransactionCreated
  UnitWebhook'TransactionUpdated _ -> UnitEvent'TransactionUpdated
  UnitUnhandledWebhook t _ -> t
