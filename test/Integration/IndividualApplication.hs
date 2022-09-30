module Integration.IndividualApplication (spec) where

import Data.Generics.Labels ()
import Fixtures.Client (UnitAPI (..))
import Fixtures.GoldenFile (goldenIndividualApplicationRequest)
import Lens.Micro ((&), (.~), (^.))
import Test.Hspec (SpecWith, describe, it, shouldBe)
import TheUnit.Model.Application.IndividualApplication
  ( IndividualApplicationResponse (..),
    _applicationId,
  )
import TheUnit.Model.Response (UnitResponse (UnitResponseData))

spec :: SpecWith UnitAPI
spec = do
  describe "API: CreateIndividualApplication" do
    --
    it "Approve" \UnitAPI {createIndividualApplication} -> do
      fc <- goldenIndividualApplicationRequest
      resp <- createIndividualApplication fc
      case resp of
        Left e -> error $ "Got HTTP error: " <> show e
        Right (UnitResponseData (IndividualApplicationResponse'Approved _)) -> pure () -- success
        Right r -> error $ "Expect application approve, but got: " <> show r
    --
    -- https://docs.unit.co/applications#testing-applications
    -- SEE: https://docs.unit.co/simulations#simulate-application-statuses
    it "Denied" \UnitAPI {createIndividualApplication} -> do
      fc <- goldenIndividualApplicationRequest
      -- to get Denied status - we should set SSN 000000001
      let deniedRequest = fc & #individualApplication . #ssn .~ "000000001"
      resp <- createIndividualApplication deniedRequest
      case resp of
        Left e -> error $ "Got HTTP error: " <> show e
        Right (UnitResponseData (IndividualApplicationResponse'Denied _)) -> pure () -- success
        Right r -> error $ "Expect application Denied, but got: " <> show r
    --
    it "PendingReview" \UnitAPI {createIndividualApplication} -> do
      fc <- goldenIndividualApplicationRequest
      -- to get PendingReview status - we should set SSN 000000004
      let pendingRequest = fc & #individualApplication . #ssn .~ "000000004"
      resp <- createIndividualApplication pendingRequest
      case resp of
        Left e -> error $ "Got HTTP error: " <> show e
        Right (UnitResponseData (IndividualApplicationResponse'PendingReview _)) -> pure () -- success
        Right r -> error $ "Expect application Denied, but got: " <> show r
    --
    it "AwaitingDocuments" \UnitAPI {createIndividualApplication} -> do
      fc <- goldenIndividualApplicationRequest
      -- to get AwaitingDocuments status - we should set SSN
      -- - 000000002 - to simulate for Address Verification
      -- - 000000003 - to simulate for Id Document
      -- - 000000006 - to simulate for Social Security Card
      let documentsRequest = fc & #individualApplication . #ssn .~ "000000006"
      resp <- createIndividualApplication documentsRequest
      case resp of
        Left e -> error $ "Got HTTP error: " <> show e
        Right (UnitResponseData (IndividualApplicationResponse'AwaitingDocuments _)) -> pure () -- success
        Right r -> error $ "Expect application Denied, but got: " <> show r
  ---
  --
  describe "API: GetIndividualApplication" do
    it "get-by-id" \UnitAPI {createIndividualApplication', getIndividualApplicationById} -> do
      fc <- goldenIndividualApplicationRequest
      application <- createIndividualApplication' fc
      resp <- getIndividualApplicationById (application ^. _applicationId)
      case resp of
        Left e -> error $ "Got HTTP error: " <> show e
        Right (UnitResponseData application') -> application' `shouldBe` application
        Right r -> error $ "Expect application Denied, but got: " <> show r
