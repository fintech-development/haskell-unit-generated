{-# LANGUAGE DeriveAnyClass #-}

module TheUnit.Model.Payment.PaymentStatus
  ( Direction (..),
    PaymentStatus (..),
    RecurringPaymentStatus (..),
    ReceivedPaymentStatus (..),
  )
where

import qualified Data.Aeson as J
import Data.Aeson.Deriving
import qualified Data.OpenApi as OpenApi
import GHC.Generics (Generic)

-- | Payment Direction
data Direction
  = Direction'Debit
  | Direction'Credit
  deriving (Show, Eq, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving
    (J.ToJSON, J.FromJSON)
    via GenericEncoded
          '[ConstructorTagModifier := DropPrefix "Direction'"]
          Direction

-- | Payment Status
-- See [Payment Status](https://developers.unit.co/#ach-status).
data PaymentStatus
  = PaymentStatus'Pending
  | PaymentStatus'PendingReview
  | PaymentStatus'Rejected
  | PaymentStatus'Clearing
  | PaymentStatus'Sent
  | PaymentStatus'Canceled
  | PaymentStatus'Returned
  deriving (Show, Eq, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving
    (J.ToJSON, J.FromJSON)
    via GenericEncoded
          '[ConstructorTagModifier := DropPrefix "PaymentStatus'"]
          PaymentStatus

data RecurringPaymentStatus
  = RecurringPaymentStatus'Active
  | RecurringPaymentStatus'Completed
  | RecurringPaymentStatus'Disabled
  deriving (Show, Eq, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving
    (J.ToJSON, J.FromJSON)
    via GenericEncoded
          '[ConstructorTagModifier := DropPrefix "RecurringPaymentStatus'"]
          RecurringPaymentStatus

data ReceivedPaymentStatus
  = ReceivedPaymentStatus'Pending
  | ReceivedPaymentStatus'Advanced
  | ReceivedPaymentStatus'Completed
  | ReceivedPaymentStatus'Returned
  deriving (Show, Eq, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving
    (J.ToJSON, J.FromJSON)
    via GenericEncoded
          '[ConstructorTagModifier := DropPrefix "ReceivedPaymentStatus'"]
          ReceivedPaymentStatus
