{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Integration.CreateIndividualApplication (spec) where

import Control.Lens ((&), (.~))
import qualified Data.Aeson as J
import Data.Generics.Labels ()
import Fixtures.Client (Client (..))
import Fixtures.Helpers (uuidV4)
import Test.Hspec (SpecWith, describe, it)
import qualified TheUnit.API.Application.CreateIndividualApplication as API
import TheUnit.Model.Application.IndividualApplication
  ( CreateIndividualApplicationRequest,
    CreateIndividualApplicationResponse (..),
  )
import TheUnit.Model.Envelope (UnitEnvelope (..))
import TheUnit.Model.Response (UnitResponse (UnitResponseData))

loadGoldFile :: IO CreateIndividualApplicationRequest
loadGoldFile = do
  Just (UnitEnvelope fc) <- J.decodeFileStrict' "./test/Fixtures/goldenfiles/individualApplication/ApplicationRequest.json"
  newIdempotencyKey <- uuidV4
  pure $ fc & #idempotencyKey .~ newIdempotencyKey

spec :: SpecWith (Client _ _ _ _)
spec =
  describe "API: CreateIndividualApplication" do
    --
    it "Approve" \Client {performRequest} -> do
      fc <- loadGoldFile
      let req = API.createIndividualApplication fc
      resp <- performRequest req
      case resp of
        Left e -> error $ "Got HTTP error: " <> show e
        Right (UnitResponseData (CreateIndividualApplicationResponse'Approved _)) -> pure () -- success
        Right r -> error $ "Expect application approve, but got: " <> show r
    -- https://docs.unit.co/applications#testing-applications
    -- SEE: https://docs.unit.co/simulations#simulate-application-statuses
    it "Denied" \Client {performRequest} -> do
      fc <- loadGoldFile
      -- to get Denied status - we should set SSN 000000001
      let deniedRequest = fc & #individualApplication . #ssn .~ "000000001"
      resp <- performRequest (API.createIndividualApplication deniedRequest)
      case resp of
        Left e -> error $ "Got HTTP error: " <> show e
        Right (UnitResponseData CreateIndividualApplicationResponse'Denied) -> pure () -- success
        Right r -> error $ "Expect application Denied, but got: " <> show r
    --
    it "PendingReview" \Client {performRequest} -> do
      fc <- loadGoldFile
      -- to get PendingReview status - we should set SSN 000000004
      let pendingRequest = fc & #individualApplication . #ssn .~ "000000004"
      resp <- performRequest (API.createIndividualApplication pendingRequest)
      case resp of
        Left e -> error $ "Got HTTP error: " <> show e
        Right (UnitResponseData (CreateIndividualApplicationResponse'PendingReview _)) -> pure () -- success
        Right r -> error $ "Expect application Denied, but got: " <> show r
    --
    it "AwaitingDocuments" \Client {performRequest} -> do
      fc <- loadGoldFile
      -- to get AwaitingDocuments status - we should set SSN
      -- - 000000002 - to simulate for Address Verification
      -- - 000000003 - to simulate for Id Document
      -- - 000000006 - to simulate for Social Security Card
      let documentsRequest = fc & #individualApplication . #ssn .~ "000000006"
      resp <- performRequest (API.createIndividualApplication documentsRequest)
      case resp of
        Left e -> error $ "Got HTTP error: " <> show e
        Right (UnitResponseData (CreateIndividualApplicationResponse'AwaitingDocuments _)) -> pure () -- success
        Right r -> error $ "Expect application Denied, but got: " <> show r
