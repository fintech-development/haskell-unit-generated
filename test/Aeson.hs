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
    CreateIndividualApplicationResponse (..),
    IndividualApplicationApproved (..),
    IndividualApplicationAwaitingDocuments (..),
    IndividualApplicationCanceled (..),
    IndividualApplicationDenied (..),
    IndividualApplicationPending (..),
    UnitDepositAccount,
    UnitEnvelope,
    UnitResponse (..),
  )
import qualified TheUnit.Model.Payment.BookPayment as Payment
import TheUnit.Model.Relationships

spec :: Spec
spec = do
  describe "AesonInstances" do
    describe "Application" do
      applicationRequest
      applicationResponse
    depositAccount
    payments

--

applicationRequest :: Spec
applicationRequest = do
  it "CreateIndividualApplicationRequest" do
    verifyJSON @(UnitEnvelope CreateIndividualApplicationRequest) $ Fixtures.IndividualApplication File'Request

applicationResponse :: Spec
applicationResponse = describe "CreateIndividualApplicationResponse" do
  let approved' = CreateIndividualApplicationResponse'Approved IndividualApplicationApproved {applicationId = "593341", customer = CustomerId "512138"}
  let awaitingDocuments = CreateIndividualApplicationResponse'AwaitingDocuments IndividualApplicationAwaitingDocuments {applicationId = "53"}
  let pending' = CreateIndividualApplicationResponse'Pending IndividualApplicationPending {applicationId = "595420"}
  let pendingReview' = CreateIndividualApplicationResponse'PendingReview IndividualApplicationPending {applicationId = "595421"}
  let canceled = CreateIndividualApplicationResponse'Canceled IndividualApplicationCanceled {applicationId = "1"}
  let denied = CreateIndividualApplicationResponse'Denied IndividualApplicationDenied {applicationId = "2"}

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
      let res :: Either String (UnitResponse CreateIndividualApplicationResponse) = J.eitherDecode' rawJsonData
      let resp = fromLeftError "JsonDecode" res

      case resp of
        UnitErrorResponse _errs -> error "Expect Response data, but get error"
        UnitResponseData payload -> payload `shouldBe` expected

--

depositAccount :: Spec
depositAccount = do
  it "DepositAccount" do
    verifyJSON @(UnitEnvelope UnitDepositAccount) (DepositAccount File'DepositAccount)
  --
  it "CreateDepositAccountData" do
    verifyJSON @(UnitEnvelope CreateDepositAccountData) (DepositAccount File'CreateDepositAccountData)

payments :: Spec
payments = do
  it "BookPayment" do
    verifyJSON @(UnitEnvelope Payment.BookPayment) (Payments File'BookPayment)
