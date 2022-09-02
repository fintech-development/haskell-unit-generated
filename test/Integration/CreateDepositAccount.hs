{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Integration.CreateDepositAccount (spec) where

import Fixtures.Client (UnitAPI (..))
import Fixtures.GoldenFile (goldenIndividualApplicationRequest)
import Fixtures.Helpers (uuidV4)
import Lens.Micro ((^.))
import Test.Hspec (SpecWith, describe, it, shouldBe)
import qualified TheUnit.Model as Model
import TheUnit.Model.Customer.IndividualCustomer (IndividualCustomerId)

approveApplication :: UnitAPI -> IO IndividualCustomerId
approveApplication UnitAPI {createIndividualApplication} = do
  fc <- goldenIndividualApplicationRequest
  resp <- createIndividualApplication fc
  case resp of
    Left e -> error $ "Got HTTP error: " <> show e
    Right (Model.UnitResponseData (Model.CreateIndividualApplicationResponse'Approved res)) -> pure $ res ^. #customer
    Right r -> error $ "Expect application approve, but got: " <> show r

spec :: SpecWith UnitAPI
spec =
  describe "API: CreateDepositAccount" do
    --
    it "should create new DepositAccount for Approved Customer" \unitAPI@UnitAPI {createDepositAccount'} -> do
      customerId <- approveApplication unitAPI

      idempotencyKey <- uuidV4
      let req =
            Model.CreateDepositAccountData
              { depositProduct = Model.UnitDepositProduct'Checking,
                idempotencyKey,
                tags = Just [("test_case", "CreateDepositAccount")],
                customer = customerId
              }
      resp <- createDepositAccount' req
      resp ^. #customer `shouldBe` customerId
      resp ^. #status `shouldBe` Model.UnitDepositAccountStatus'Open
