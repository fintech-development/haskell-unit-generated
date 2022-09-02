{-# LANGUAGE DeriveAnyClass #-}

module TheUnit.Model.Accounts.UnitDepositProduct
  ( UnitDepositProduct (..),
  )
where

import qualified Data.Aeson as J
import Data.Aeson.Deriving
import qualified Data.OpenApi as OpenApi
import GHC.Generics (Generic)

data UnitDepositProduct
  = UnitDepositProduct'Checking
  | UnitDepositProduct'Savings
  deriving (Show, Eq, Generic)
  deriving anyclass (OpenApi.ToSchema)
  deriving
    (J.ToJSON, J.FromJSON)
    via GenericEncoded
          '[ConstructorTagModifier := [Lowercase, DropPrefix "UnitDepositProduct'"]]
          UnitDepositProduct
