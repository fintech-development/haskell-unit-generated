module TheUnit.Model.Core where

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.Text as T
import Prelude as P

-- | Removes Null fields.  (OpenAPI-Specification 2.0 does not allow Null in JSON)
_omitNulls :: [(T.Text, J.Value)] -> J.Value
_omitNulls = J.object . P.filter notNull
  where
    notNull (_, J.Null) = False
    notNull _ = True

(.->) :: J.FromJSON a => J.Parser J.Object -> T.Text -> J.Parser a
(.->) parser key = do
  obj <- parser
  obj .: key

(.->?) :: J.FromJSON a => J.Parser J.Object -> T.Text -> J.Parser (Maybe a)
(.->?) parser key = do
  obj <- parser
  obj .:? key
