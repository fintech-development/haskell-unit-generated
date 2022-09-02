{-# LANGUAGE AllowAmbiguousTypes #-}

module Aeson.DepositAccount (spec) where

import Fixtures.GoldenFile (DepositAccountFiles (File'CreateDepositAccountData, File'DepositAccount), GoldenFile (DepositAccount))
import Fixtures.VerifyJSON (verifyJSON)
import Test.Hspec (Spec, it)
import TheUnit.Model (CreateDepositAccountData, UnitDepositAccount, UnitEnvelope)

spec :: Spec
spec = do
  it "DepositAccount" do
    verifyJSON @(UnitEnvelope UnitDepositAccount) (DepositAccount File'DepositAccount)
  --
  it "CreateDepositAccountData" do
    verifyJSON @(UnitEnvelope CreateDepositAccountData) (DepositAccount File'CreateDepositAccountData)
