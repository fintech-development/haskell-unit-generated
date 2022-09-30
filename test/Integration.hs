module Integration (spec) where

import Data.Foldable (traverse_)
import qualified Fixtures.Client as Fixtures
import qualified Integration.CreateDepositAccount as CreateDepositAccount
import qualified Integration.IndividualApplication as IndividualApplication
import qualified Integration.Payments as Payments
import LoadEnv (loadEnv)
import LoadEnv.Parse (parseEnvironment)
import System.Environment (setEnv)
import Test.Hspec (Spec, beforeAll, describe)
import Text.Parsec.String (parseFromFile)

updateEnv :: IO ()
updateEnv = do
  Right env <- parseFromFile parseEnvironment ".env"
  traverse_ (uncurry setEnv) env
  loadEnv

spec :: Spec
spec =
  describe "Integration" do
    beforeAll (updateEnv >> Fixtures.makeAPI) do
      IndividualApplication.spec
      CreateDepositAccount.spec
      Payments.spec
