module Aeson.Application (spec) where

import Control.Monad (forM_)
import Data.Aeson as J
import Data.Generics.Labels ()
import Fixtures
import Test.Hspec (Spec, describe, it, shouldBe)
import TheUnit.Model.Application (ApplicationStatus (..), CreateIndividualApplicationResponse)
import TheUnit.Model.Application.IndividualApplication
  ( CreateIndividualApplicationRequest,
    CreateIndividualApplicationResponse (..),
    IndividualApplicationApproved (..),
    IndividualApplicationAwaitingDocuments (..),
    IndividualApplicationCanceled (..),
    IndividualApplicationDenied (..),
    IndividualApplicationPending (..),
  )
import TheUnit.Model.Customer.IndividualCustomer (IndividualCustomerId (IndividualCustomerId))
import TheUnit.Model.Response (UnitEnvelope, UnitResponse (..))

spec :: Spec
spec = do
  applicationRequest
  applicationResponse

applicationRequest :: Spec
applicationRequest = do
  it "CreateIndividualApplicationRequest" do
    verifyJSON @(UnitEnvelope CreateIndividualApplicationRequest) $ IndividualApplication File'Request

applicationResponse :: Spec
applicationResponse = describe "CreateIndividualApplicationResponse" do
  let approved' = CreateIndividualApplicationResponse'Approved IndividualApplicationApproved {applicationId = "593341", customer = IndividualCustomerId "512138"}
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
      rawJsonData <- goldenFile $ IndividualApplication goldenFile'
      let res :: Either String (UnitResponse CreateIndividualApplicationResponse) = J.eitherDecode' rawJsonData
      let resp = fromLeftError "JsonDecode" res

      case resp of
        UnitErrorResponse _errs -> error "Expect Response data, but get error"
        UnitResponseData payload -> payload `shouldBe` expected

--
