{-# LANGUAGE DeriveAnyClass #-}

module TheUnit.Model.Accounts.UnitDepositProduct
  ( UnitDepositProduct (..),
  )
where

import qualified Data.Aeson as J
import Data.Aeson.Deriving
import qualified Data.OpenApi as OpenApi
import GHC.Generics (Generic)

--  | [Unit deposit product](https://docs.unit.co/deposit-accounts#deposit-products)
-- The deposit product defines the set of terms that are applied to the account:
-- Deposit products are created and updated by Unit's compliance team
--
-- By default only Checking as available
data UnitDepositProduct
  = UnitDepositProduct'Checking
  | UnitDepositProduct'UNKNOWN
  deriving (Show, Eq, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving
    (J.ToJSON, J.FromJSON)
    via GenericEncoded
          '[ConstructorTagModifier := [Lowercase, DropPrefix "UnitDepositProduct'"]]
          UnitDepositProduct
