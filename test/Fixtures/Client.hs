module Fixtures.Client (makeClient, Client (..)) where

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
import Network.Integrated.HTTP.MimeTypes (MimeType, MimeUnrender, Produces)
import qualified System.Environment as Env

setAuth :: BS.ByteString -> Core.ClientConfig -> Core.ClientConfig
setAuth token = (`Core.addAuthMethod` Auth20BearerToken token)

newtype Client req res accept contentType = Client {performRequest :: Core.Request req contentType res accept -> IO (Either MimeError res)}

makeClient ::
  forall req res accept contentType.
  (Produces req accept, MimeUnrender accept res, MimeType contentType) =>
  IO (Client req res accept contentType)
makeClient = do
  token <- encode <$> Env.getEnv "UNIT_ORG_AUTH_TOKEN"
  host <- encodeL <$> Env.getEnv "UNIT_API_HOST"
  cfg <- Core.newConfig host <&> setAuth token

  manager <- HTTP.newTlsManager
  let dispatchClient = makeDispatchClient manager cfg
  pure $ Client (Dispatch.dispatchMime' dispatchClient)

encode :: String -> BS.ByteString
encode = T.encodeUtf8 . T.pack

encodeL :: String -> BL.ByteString
encodeL = BL.fromStrict . encode
