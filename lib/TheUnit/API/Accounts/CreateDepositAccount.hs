module TheUnit.API.Accounts.CreateDepositAccount
  ( createDepositAccount,
  )
where

import qualified Data.Proxy as P
import Network.Integrated.HTTP.Auth (Auth20BearerToken)
import Network.Integrated.HTTP.Core (HasBodyParam)
import qualified Network.Integrated.HTTP.Core as Core
import Network.Integrated.HTTP.MimeTypes (Consumes, MimeVndApiJSON, Produces)
import TheUnit.Model.Accounts.DepositAccount (UnitDepositAccount)
import qualified TheUnit.Model.Accounts.DepositAccount as Model
import TheUnit.Model.Response (UnitEnvelope (..), UnitResponse)

data CreateDepositAccountRequest

instance Produces CreateDepositAccountRequest MimeVndApiJSON

instance Consumes CreateDepositAccountRequest MimeVndApiJSON

instance HasBodyParam CreateDepositAccountRequest (UnitEnvelope Model.CreateDepositAccountData)

-- * Operations

-- *** Create Deposit Account

-- | @POST \/applications@
--
-- AuthMethod: 'Auth20BearerToken'
createDepositAccount ::
  Model.CreateDepositAccountData ->
  Core.Request CreateDepositAccountRequest MimeVndApiJSON (UnitResponse UnitDepositAccount) MimeVndApiJSON
createDepositAccount requestData =
  Core._mkRequest "POST" ["/accounts"]
    `Core._hasAuthType` (P.Proxy :: P.Proxy Auth20BearerToken)
    `Core.setBodyParam` UnitEnvelope requestData
