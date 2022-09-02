module Fixtures.Client (makeAPI, UnitAPI (..)) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Functor ((<&>))
import Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Client.TLS as HTTP
import Network.Integrated.HTTP.Auth (Auth20BearerToken (..))
import Network.Integrated.HTTP.Client (MimeError)
import qualified Network.Integrated.HTTP.Core as Core
import Network.Integrated.HTTP.DispatchClient (makeDispatchClient)
import qualified Network.Integrated.HTTP.DispatchClient as Dispatch
import qualified System.Environment as Env
import qualified TheUnit.API as API
import qualified TheUnit.Model as Model

type APIResponse a = IO (Either MimeError (Model.UnitResponse a))

data UnitAPI = UnitAPI
  { createIndividualApplication :: Model.CreateIndividualApplicationRequest -> APIResponse Model.CreateIndividualApplicationResponse,
    -- | strict version of `createIndividualApplication`
    createIndividualApplication' :: Model.CreateIndividualApplicationRequest -> IO Model.CreateIndividualApplicationResponse,
    --
    createDepositAccount :: Model.CreateDepositAccountData -> APIResponse Model.UnitDepositAccount,
    -- | strict version of `createDepositAccount
    createDepositAccount' :: Model.CreateDepositAccountData -> IO Model.UnitDepositAccount
  }

makeAPI :: IO UnitAPI
makeAPI = do
  token <- encode <$> Env.getEnv "UNIT_ORG_AUTH_TOKEN"
  host <- encodeL <$> Env.getEnv "UNIT_API_HOST"
  cfg <- Core.newConfig host <&> setAuth token

  manager <- HTTP.newTlsManager
  let createIndividualApplication =
        Dispatch.dispatchMime' (makeDispatchClient manager cfg) . API.createIndividualApplication
  let createDepositAccount =
        Dispatch.dispatchMime' (makeDispatchClient manager cfg) . API.createDepositAccount

  pure $
    UnitAPI
      { createIndividualApplication,
        createIndividualApplication' = fmap handleResponse . createIndividualApplication,
        createDepositAccount,
        createDepositAccount' = fmap handleResponse . createDepositAccount
      }
  where
    handleResponse :: Either MimeError (Model.UnitResponse a) -> a
    handleResponse resp =
      case resp of
        Left e -> error $ "Got HTTP error: " <> show e
        Right (Model.UnitErrorResponse errs) -> error $ "Expect successful response, but got error: " <> show errs
        Right (Model.UnitResponseData a) -> a

setAuth :: BS.ByteString -> Core.ClientConfig -> Core.ClientConfig
setAuth token = (`Core.addAuthMethod` Auth20BearerToken token)

encode :: String -> BS.ByteString
encode = T.encodeUtf8 . T.pack

encodeL :: String -> BL.ByteString
encodeL = BL.fromStrict . encode
