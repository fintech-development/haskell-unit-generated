{-# LANGUAGE DeriveAnyClass #-}

module TheUnit.Model.Application.ApplicationType (ApplicationType (..)) where

import Control.Arrow (left)
import Data.Aeson (FromJSON (parseJSON))
import qualified Data.Aeson as J
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.Integrated.HTTP.MimeTypes
import qualified Web.HttpApiData as WH

data ApplicationType
  = ApplicationType'IndividualApplication
  | ApplicationType'BusinessApplication
  | ApplicationType'TrustApplication
  deriving (Show, Eq, Generic)
  deriving anyclass (OpenApi.ToSchema)

instance J.ToJSON ApplicationType where toJSON = J.toJSON . fromE'Type

instance J.FromJSON ApplicationType where parseJSON o = either fail pure . toE'Type =<< J.parseJSON o

instance WH.ToHttpApiData ApplicationType where toQueryParam = WH.toQueryParam . fromE'Type

instance WH.FromHttpApiData ApplicationType where parseQueryParam o = WH.parseQueryParam o >>= left T.pack . toE'Type

instance MimeRender MimeMultipartFormData ApplicationType where mimeRender _ = mimeRenderDefaultMultipartFormData

-- | unwrap 'ApplicationType' enum
fromE'Type :: ApplicationType -> T.Text
fromE'Type = \case
  ApplicationType'IndividualApplication -> "individualApplication"
  ApplicationType'BusinessApplication -> "businessApplication"
  ApplicationType'TrustApplication -> "trustApplication"

-- | parse 'ApplicationType' enum
toE'Type :: T.Text -> Either String ApplicationType
toE'Type = \case
  "individualApplication" -> Right ApplicationType'IndividualApplication
  "businessApplication" -> Right ApplicationType'BusinessApplication
  "trustApplication" -> Right ApplicationType'TrustApplication
  s -> Left $ "enum parse failure: " ++ show s
