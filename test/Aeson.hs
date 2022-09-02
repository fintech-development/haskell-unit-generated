module Aeson (spec) where

import qualified Aeson.Application as Application (spec)
import qualified Aeson.DepositAccount as DepositAccount
import Test.Hspec (Spec, describe)

spec :: Spec
spec = do
  describe "AesonInstances" do
    Application.spec
    DepositAccount.spec
