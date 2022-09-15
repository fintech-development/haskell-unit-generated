module TheUnit.API.ACHPayment.CreateACHPayment
  ( createACHPaymentWithAccountAndRouting,
    createACHPaymentWithPlaidProcessorToken,
    createACHPaymentWithLinkedCounterparty,
  )
where

import qualified Data.Proxy as P
import Network.Integrated.HTTP.Auth (Auth20BearerToken)
import Network.Integrated.HTTP.Core (HasBodyParam)
import qualified Network.Integrated.HTTP.Core as Core
import Network.Integrated.HTTP.MimeTypes (Consumes, MimeVndApiJSON, Produces)
import TheUnit.Model.Payment.ACHPayment
  ( ACHPayment,
    CreateInlinePaymentRequest,
    CreateLinkedPaymentRequest,
    CreateVerifiedPaymentRequest,
  )
import TheUnit.Model.Response (UnitEnvelope (..), UnitResponse)

data CreateACHPaymentRequest

instance Produces CreateACHPaymentRequest MimeVndApiJSON

instance Consumes CreateACHPaymentRequest MimeVndApiJSON

instance HasBodyParam CreateACHPaymentRequest (UnitEnvelope CreateInlinePaymentRequest)

instance HasBodyParam CreateACHPaymentRequest (UnitEnvelope CreateLinkedPaymentRequest)

instance HasBodyParam CreateACHPaymentRequest (UnitEnvelope CreateVerifiedPaymentRequest)

type CoreRequest req resp =
  Core.Request req MimeVndApiJSON (UnitResponse resp) MimeVndApiJSON

-- * Operations

-- *** Create ACH Payment with inline Counterparty (Account and Routing numbers)

-- | @POST \/payments@
--
-- AuthMethod: 'Auth20BearerToken'
-- Originates an ACH payment to a counterparty which is specified inline (vs to a linked Counterparty Resource).
-- NOTE:
-- Originating ACH debits requires capturing the authorization of the account owner and therefore originating ACH debits to inline counterparties is not allowed by default. If your use case requires this capability, please contact Unit.
-- [Doc](https://docs.unit.co/ach-origination/#payment-inline-counterparty)
createACHPaymentWithAccountAndRouting :: CreateInlinePaymentRequest -> CoreRequest CreateACHPaymentRequest ACHPayment
createACHPaymentWithAccountAndRouting requestData =
  Core._mkRequest "POST" ["/payments"]
    `Core._hasAuthType` (P.Proxy :: P.Proxy Auth20BearerToken)
    `Core.setBodyParam` UnitEnvelope requestData

--

-- *** Create ACH Payment with Plaid Processor Token

-- | @POST \/payments@
--
-- AuthMethod: 'Auth20BearerToken'
-- Originates an ACH payment to a counterparty which is verified by [Plaid](https://plaid.com/).
-- INFO:
-- For more information on using Plaid, please read Unit's [Plaid partnership guide](https://guides.unit.co/partnerships/plaid)
-- [Doc](https://docs.unit.co/ach-origination/#create-ach-payment-with-plaid-token)
createACHPaymentWithPlaidProcessorToken :: CreateVerifiedPaymentRequest -> CoreRequest CreateACHPaymentRequest ACHPayment
createACHPaymentWithPlaidProcessorToken requestData =
  Core._mkRequest "POST" ["/payments"]
    `Core._hasAuthType` (P.Proxy :: P.Proxy Auth20BearerToken)
    `Core.setBodyParam` UnitEnvelope requestData

-- *** Create ACH Payment with Plaid Processor Token

-- | @POST \/payments@
--
-- AuthMethod: 'Auth20BearerToken'
-- Originates an ACH payment to a [Counterparty](https://docs.unit.co/resources#counterparty-resource). The counterparty should be created separately through [Create Counterparty](https://docs.unit.co/payments-counterparties#create-counterparty).
-- [Doc](https://docs.unit.co/ach-origination/#payment-linked-counterparty)
createACHPaymentWithLinkedCounterparty :: CreateLinkedPaymentRequest -> CoreRequest CreateACHPaymentRequest ACHPayment
createACHPaymentWithLinkedCounterparty requestData =
  Core._mkRequest "POST" ["/payments"]
    `Core._hasAuthType` (P.Proxy :: P.Proxy Auth20BearerToken)
    `Core.setBodyParam` UnitEnvelope requestData
