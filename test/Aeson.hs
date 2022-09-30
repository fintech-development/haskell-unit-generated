{-# LANGUAGE AllowAmbiguousTypes #-}

module Aeson (spec) where

import Control.Monad (forM_)
import qualified Data.Aeson as J
import Fixtures
import Test.Hspec (Spec, describe, it, shouldBe)
import TheUnit.Model
  ( ApplicationStatus (..),
    CreateDepositAccountData,
    CreateIndividualApplicationRequest,
    IndividualApplicationApproved (..),
    IndividualApplicationAwaitingDocuments (..),
    IndividualApplicationCanceled (..),
    IndividualApplicationDenied (..),
    IndividualApplicationPending (..),
    IndividualApplicationResponse (..),
    UnitDepositAccount,
    UnitEnvelope (..),
    UnitResponse (..),
  )
import qualified TheUnit.Model as Webhooks
import qualified TheUnit.Model.Payment as Payment
import TheUnit.Model.Relationships
import qualified TheUnit.Model.Webhooks as Webhook

spec :: Spec
spec = do
  describe "AesonInstances" do
    describe "Application" do
      applicationRequest
      applicationResponse
    depositAccount
    payments
    webhooks

--

applicationRequest :: Spec
applicationRequest = do
  it "CreateIndividualApplicationRequest" do
    verifyJSON_ @(UnitEnvelope CreateIndividualApplicationRequest) $ Fixtures.IndividualApplication File'Request

applicationResponse :: Spec
applicationResponse = describe "CreateIndividualApplicationResponse" do
  let approved' = IndividualApplicationResponse'Approved IndividualApplicationApproved {applicationId = "593341", customer = CustomerId "512138"}
  let awaitingDocuments = IndividualApplicationResponse'AwaitingDocuments IndividualApplicationAwaitingDocuments {applicationId = "53"}
  let pending' = IndividualApplicationResponse'Pending IndividualApplicationPending {applicationId = "595420"}
  let pendingReview' = IndividualApplicationResponse'PendingReview IndividualApplicationPending {applicationId = "595421"}
  let canceled = IndividualApplicationResponse'Canceled IndividualApplicationCanceled {applicationId = "1"}
  let denied = IndividualApplicationResponse'Denied IndividualApplicationDenied {applicationId = "2"}

  let table =
        [ (Approved, File'ResponseApproved, approved'),
          (AwaitingDocuments, File'ResponseAwaitingDocuments, awaitingDocuments),
          (Pending, File'ResponsePending, pending'),
          (PendingReview, File'ResponsePendingReview, pendingReview'),
          (Canceled, File'ResponseCanceled, canceled),
          (Denied, File'ResponseDenied, denied)
        ]
  forM_ table \(status, goldenFile', expected) ->
    it ("Decode response: " <> show status) do
      rawJsonData <- goldenFile $ Fixtures.IndividualApplication goldenFile'
      let res :: Either String (UnitResponse IndividualApplicationResponse) = J.eitherDecode' rawJsonData
      let resp = fromLeftError "JsonDecode" res

      case resp of
        UnitErrorResponse _errs -> error "Expect Response data, but get error"
        UnitResponseData payload -> payload `shouldBe` expected

--

depositAccount :: Spec
depositAccount = do
  it "DepositAccount" do
    verifyJSON_ @(UnitEnvelope UnitDepositAccount) (DepositAccount File'DepositAccount)
  --
  it "CreateDepositAccountData" do
    verifyJSON_ @(UnitEnvelope CreateDepositAccountData) (DepositAccount File'CreateDepositAccountData)

payments :: Spec
payments = do
  describe "Payments" do
    it "BookPayment" do
      verifyJSON_ @(UnitEnvelope Payment.BookPayment) (Payments File'BookPayment)
    it "ACHCreditPayment" do
      verifyJSON_ @(UnitEnvelope Payment.ACHPayment) (Payments File'ACHCreditPayment)
    it "ACHDebitPayment" do
      verifyJSON_ @(UnitEnvelope Payment.ACHPayment) (Payments File'ACHDebitPayment)
    it "CreateInlinePaymentRequest" do
      verifyJSON_ @(UnitEnvelope Payment.CreateInlinePaymentRequest) (Payments File'CreateInlinePaymentRequest)
    it "CreateLinkedPaymentRequest" do
      verifyJSON_ @(UnitEnvelope Payment.CreateLinkedPaymentRequest) (Payments File'CreateLinkedPaymentRequest)
    it "CreateVerifiedPaymentRequest" do
      verifyJSON_ @(UnitEnvelope Payment.CreateVerifiedPaymentRequest) (Payments File'CreateVerifiedPaymentRequest)

webhooks :: Spec
webhooks = do
  describe "Webhooks" do
    it "AccountCreated" do
      UnitEnvelope {payload = [accountCreated]} <- verifyJSON @(UnitEnvelope [Webhook.UnitWebhook]) (Webhooks File'AccountCreated)
      Webhooks.webhookEvent accountCreated `shouldBe` Webhooks.UnitEvent'AccountCreated

    --
    it "ApplicationAwaitingDocuments" do
      UnitEnvelope {payload = [webhook]} <- verifyJSON @(UnitEnvelope [Webhook.UnitWebhook]) (Webhooks File'ApplicationAwaitingDocuments)
      Webhooks.webhookEvent webhook `shouldBe` Webhooks.UnitEvent'ApplicationAwaitingDocuments

    --
    it "ReceivedPaymentCreated" do
      UnitEnvelope {payload = [webhook]} <- verifyJSON @(UnitEnvelope [Webhook.UnitWebhook]) (Webhooks File'ReceivedPaymentCreated)
      Webhooks.webhookEvent webhook `shouldBe` Webhooks.UnitEvent'ReceivedPaymentCreated
