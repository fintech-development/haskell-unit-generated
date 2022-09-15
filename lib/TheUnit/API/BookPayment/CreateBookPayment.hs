module TheUnit.API.BookPayment.CreateBookPayment
  ( createBookPayment,
  )
where

import qualified Data.Proxy as P
import Network.Integrated.HTTP.Auth (Auth20BearerToken)
import Network.Integrated.HTTP.Core (HasBodyParam)
import qualified Network.Integrated.HTTP.Core as Core
import Network.Integrated.HTTP.MimeTypes (Consumes, MimeVndApiJSON, Produces)
import TheUnit.Model.Payment.BookPayment (BookPayment, CreateBookPayment)
import TheUnit.Model.Response (UnitEnvelope (..), UnitResponse)

data CreateBookPaymentRequest

instance Produces CreateBookPaymentRequest MimeVndApiJSON

instance Consumes CreateBookPaymentRequest MimeVndApiJSON

instance HasBodyParam CreateBookPaymentRequest (UnitEnvelope CreateBookPayment)

-- * Operations

-- *** Create Book Payment

-- | @POST \/payments@
--
-- AuthMethod: 'Auth20BearerToken'
-- Book payments are free and instant fund transfers between two accounts under your organization. Common uses for book payments include:
-- - Moving funds between two accounts that belong to the same end-customer.
-- - Moving funds between two accounts that belong to different end-customers.
-- - Moving funds from clearing accounts to end-customer accounts.
-- - Moving funds from your organization's operational account to an end-customer account.
--
-- Once you create a book payment, Unit will process it instantly and free of charge. The result will be one Book Transaction in each account.
createBookPayment ::
  CreateBookPayment ->
  Core.Request CreateBookPaymentRequest MimeVndApiJSON (UnitResponse BookPayment) MimeVndApiJSON
createBookPayment requestData =
  Core._mkRequest "POST" ["/payments"]
    `Core._hasAuthType` (P.Proxy :: P.Proxy Auth20BearerToken)
    `Core.setBodyParam` UnitEnvelope requestData
