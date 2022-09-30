module TheUnit.API.Application.Individual.GetIndividualApplication
  ( getIndividualApplicationById,
  )
where

import qualified Data.Proxy as P
import qualified Data.Text as T
import Network.Integrated.HTTP.Auth (Auth20BearerToken)
import qualified Network.Integrated.HTTP.Core as Core
import Network.Integrated.HTTP.MimeTypes (Consumes, MimeVndApiJSON, Produces)
import TheUnit.Model.Application
  ( IndividualApplicationResponse,
  )
import TheUnit.Model.Response (UnitResponse)

data GetApplicationByIdRequest

instance Produces GetApplicationByIdRequest MimeVndApiJSON

instance Consumes GetApplicationByIdRequest MimeVndApiJSON

-- * Operations

-- *** Get an application resource by id

-- | @POST \/applications/{id}@
--
-- AuthMethod: 'Auth20BearerToken'
getIndividualApplicationById ::
  T.Text ->
  Core.Request GetApplicationByIdRequest MimeVndApiJSON (UnitResponse IndividualApplicationResponse) MimeVndApiJSON
getIndividualApplicationById applicationId =
  Core._mkRequest "GET" ["/applications", "/", Core.encodeLazyUtf8 applicationId]
    `Core._hasAuthType` (P.Proxy :: P.Proxy Auth20BearerToken)
