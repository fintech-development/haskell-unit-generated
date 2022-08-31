{-# LANGUAGE DeriveAnyClass #-}

module TheUnit.Model.Application.ApplicationStatus (ApplicationStatus (..)) where

import qualified Data.Aeson as J
import qualified Data.OpenApi as OpenApi
import GHC.Generics (Generic)

-- | see [Application Statuses](https://docs.unit.co/applications/#application-statuses).
data ApplicationStatus
  = -- | Certain documents are required for the process to continue. You may upload them via Upload Document.
    AwaitingDocuments
  | PendingReview
  | -- | The application is being evaluated asynchronously and a result should be available shortly. Listen for webhooks (application.denied, customer.created and application.awaitingdocuments) for the final result, or periodically query the application with Get by Id).
    Pending
  | -- | The application was approved. A Customer resource was created.
    Approved
  | -- | The application was denied. A Customer resource will not be created.
    Denied
  | -- | The application was сanceled. A Customer resource will not be created.
    Canceled
  deriving (Show, Eq, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)
