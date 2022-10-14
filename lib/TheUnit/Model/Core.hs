module TheUnit.Model.Core where

import Data.Aeson (Value (Object), (.:), (.:?))
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as J
import Prelude as P

-- | Removes Null fields.  (OpenAPI-Specification 2.0 does not allow Null in JSON)
_omitNulls :: [(J.Key, J.Value)] -> J.Value
_omitNulls = J.object . P.filter notNull
  where
    notNull (_, J.Null) = False
    notNull _ = True

(.->) :: J.FromJSON a => J.Parser J.Object -> J.Key -> J.Parser a
(.->) parser key = do
  obj <- parser
  obj .: key

(.->?) :: J.FromJSON a => J.Parser J.Object -> J.Key -> J.Parser (Maybe a)
(.->?) parser key = do
  obj <- parser
  obj .:? key

mergeAesonObjects :: [Value] -> Value
mergeAesonObjects = Object . foldr (KeyMap.union . unObject) mempty
  where
    unObject (Object o) = o
    unObject _ = error "mergeAesonObjects support only JSON objects"
