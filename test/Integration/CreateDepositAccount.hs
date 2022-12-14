{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Integration.CreateDepositAccount (spec) where

import Fixtures.Client (UnitAPI (..), withAccount, withCustomer)
import Fixtures.Helpers (uuidV4)
import Lens.Micro ((^.))
import Test.Hspec (SpecWith, describe, it, shouldBe)
import qualified TheUnit.Model as Model

spec :: SpecWith UnitAPI
spec = do
  describe "API: CreateDepositAccount" do
    --
    it "should create new DepositAccount for Approved Customer" \unitAPI@UnitAPI {createDepositAccount'} -> do
      withCustomer unitAPI \customerId -> do
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
  describe "API: accountStatements" do
    --
    it "should load empty statements" \unitAPI@UnitAPI {listAccountStatements'} ->
      withAccount unitAPI \accountId -> do
        statements <- listAccountStatements' accountId
        length statements `shouldBe` 0
