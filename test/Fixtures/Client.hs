module Fixtures.Client
  ( makeAPI,
    UnitAPI (..),
    withCustomer,
    withAccount,
  )
where

import Control.Exception.Safe (finally)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Functor ((<&>))
import Data.Text as T
import qualified Data.Text.Encoding as T
import Fixtures (uuidV4)
import qualified Fixtures.Client.Cache as Cache
import Fixtures.GoldenFile (goldenIndividualApplicationRequest)
import Lens.Micro ((^.))
import qualified Network.HTTP.Client.TLS as HTTP
import Network.Integrated.HTTP.Auth (Auth20BearerToken (..))
import Network.Integrated.HTTP.Client (MimeError)
import qualified Network.Integrated.HTTP.Core as Core
import Network.Integrated.HTTP.DispatchClient (makeDispatchClient)
import qualified Network.Integrated.HTTP.DispatchClient as Dispatch
import qualified System.Environment as Env
import qualified TheUnit.API as API
import qualified TheUnit.Model as Model
import TheUnit.Model.Relationships (AccountId (AccountId), CustomerId)

type APIResponse a = IO (Either MimeError (Model.UnitResponse a))

data UnitAPI = UnitAPI
  { createIndividualApplication :: Model.CreateIndividualApplicationRequest -> APIResponse Model.IndividualApplicationResponse,
    -- | strict version of `createIndividualApplication`
    createIndividualApplication' :: Model.CreateIndividualApplicationRequest -> IO Model.IndividualApplicationResponse,
    getIndividualApplicationById :: T.Text -> APIResponse Model.IndividualApplicationResponse,
    --
    createDepositAccount :: Model.CreateDepositAccountData -> APIResponse Model.UnitDepositAccount,
    -- | strict version of `createDepositAccount
    createDepositAccount' :: Model.CreateDepositAccountData -> IO Model.UnitDepositAccount,
    -- | create Book Payment
    createBookPayment :: Model.CreateBookPayment -> APIResponse Model.BookPayment,
    createBookPayment' :: Model.CreateBookPayment -> IO Model.BookPayment,
    listAccountStatements' :: AccountId -> IO [Model.Statement],
    __cache :: Cache.Cache
  }

makeAPI :: IO UnitAPI
makeAPI = do
  token <- encode <$> Env.getEnv "UNIT_ORG_AUTH_TOKEN"
  host <- encodeL <$> Env.getEnv "UNIT_API_HOST"
  cfg <- Core.newConfig host <&> setAuth token

  manager <- HTTP.newTlsManager
  __cache <- Cache.initCache
  let createIndividualApplication =
        Dispatch.dispatchMime' (makeDispatchClient manager cfg) . API.createIndividualApplication

  let getIndividualApplicationById =
        Dispatch.dispatchMime' (makeDispatchClient manager cfg) . API.getIndividualApplicationById

  let createDepositAccount =
        Dispatch.dispatchMime' (makeDispatchClient manager cfg) . API.createDepositAccount
  let createBookPayment =
        Dispatch.dispatchMime' (makeDispatchClient manager cfg) . API.createBookPayment
  let listAccountStatements =
        Dispatch.dispatchMime' (makeDispatchClient manager cfg) . API.listAccountStatements

  pure $
    UnitAPI
      { createIndividualApplication,
        createIndividualApplication' = fmap handleResponse . createIndividualApplication,
        getIndividualApplicationById,
        createDepositAccount,
        createDepositAccount' = fmap handleResponse . createDepositAccount,
        createBookPayment,
        createBookPayment' = fmap handleResponse . createBookPayment,
        listAccountStatements' = fmap handleResponse . listAccountStatements,
        __cache
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

-- | Get customer from Cache or create new customer
withCustomer :: UnitAPI -> (CustomerId -> IO a) -> IO a
withCustomer unitAPI@UnitAPI {__cache} action = do
  mbCustomer <- Cache.getCustomer __cache
  case mbCustomer of
    Just c -> action c `finally` Cache.putCustomer __cache c
    Nothing -> do
      newCustomerId <- _approveApplication unitAPI
      Cache.putCustomer __cache newCustomerId
      withCustomer unitAPI action

_approveApplication :: UnitAPI -> IO CustomerId
_approveApplication UnitAPI {createIndividualApplication} = do
  fc <- goldenIndividualApplicationRequest
  resp <- createIndividualApplication fc
  case resp of
    Left e -> error $ "Got HTTP error: " <> show e
    Right (Model.UnitResponseData (Model.IndividualApplicationResponse'Approved res)) -> pure $ res ^. #customer
    Right r -> error $ "Expect application approve, but got: " <> show r

withAccount :: UnitAPI -> (AccountId -> IO a) -> IO a
withAccount unitAPI@UnitAPI {__cache} action = do
  mbAccount <- Cache.getAccount __cache
  case mbAccount of
    Just account -> action account `finally` Cache.putAccount __cache account
    Nothing -> withCustomer unitAPI \customerId -> do
      newAccount <- _createDepositAccount unitAPI customerId
      Cache.putAccount __cache newAccount
      withAccount unitAPI action

_createDepositAccount :: UnitAPI -> CustomerId -> IO AccountId
_createDepositAccount UnitAPI {createDepositAccount'} customerId = do
  idempotencyKey <- uuidV4
  let req =
        Model.CreateDepositAccountData
          { depositProduct = Model.UnitDepositProduct'Checking,
            idempotencyKey,
            tags = Nothing,
            customer = customerId
          }
  resp <- createDepositAccount' req
  pure $ AccountId (resp ^. #accountId)
