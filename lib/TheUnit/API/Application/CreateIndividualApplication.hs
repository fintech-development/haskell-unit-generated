module TheUnit.API.Application.CreateIndividualApplication (createIndividualApplication) where

import qualified Data.Proxy as P
import Network.Integrated.HTTP.Auth (Auth20BearerToken)
import Network.Integrated.HTTP.Core (HasBodyParam)
import qualified Network.Integrated.HTTP.Core as Core
import Network.Integrated.HTTP.MimeTypes (Consumes, MimeVndApiJSON, Produces)
import TheUnit.Model.Application (CreateIndividualApplicationRequest, CreateIndividualApplicationResponse)
import TheUnit.Model.Envelope (UnitEnvelope (..))
import TheUnit.Model.Response (UnitResponse)

data CreateApplicationRequest

instance Produces CreateApplicationRequest MimeVndApiJSON

instance Consumes CreateApplicationRequest MimeVndApiJSON

instance HasBodyParam CreateApplicationRequest (UnitEnvelope CreateIndividualApplicationRequest)

-- * Operations

-- *** Create Individual Application

-- | @POST \/applications@
--
-- AuthMethod: 'Auth20BearerToken'
createIndividualApplication ::
  CreateIndividualApplicationRequest ->
  Core.Request CreateApplicationRequest MimeVndApiJSON (UnitResponse CreateIndividualApplicationResponse) MimeVndApiJSON
createIndividualApplication requestData =
  Core._mkRequest "POST" ["/applications"]
    `Core._hasAuthType` (P.Proxy :: P.Proxy Auth20BearerToken)
    `Core.setBodyParam` (UnitEnvelope requestData)
