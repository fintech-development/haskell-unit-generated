{-# LANGUAGE OverloadedLists #-}

module Integration.Payments (spec) where

import Fixtures (uuidV4)
import Fixtures.Client (UnitAPI (..), withAccount)
import Lens.Micro ((^.))
import Test.Hspec (SpecWith, describe, it, shouldBe)
import qualified TheUnit.Model as Model

spec :: SpecWith UnitAPI
spec =
  describe "API: Payments" do
    spec'createBookPayment

spec'createBookPayment :: SpecWith UnitAPI
spec'createBookPayment = describe "Book Payment" do
  it "Create BookPayment" \unitAPI@UnitAPI {createBookPayment'} -> do
    payment <- withAccount unitAPI \accountFrom ->
      withAccount unitAPI \accountTo -> do
        idempotencyKey <- uuidV4
        let req =
              Model.CreateBookPayment
                { amount = 100,
                  description = "TestCase: Create BookPayment",
                  idempotencyKey,
                  tags = [("test_case", "Create BookPayment"), ("test", "true")],
                  accountFrom,
                  accountTo
                }
        createBookPayment' req

    payment ^. #status `shouldBe` Model.PaymentStatus'Rejected
