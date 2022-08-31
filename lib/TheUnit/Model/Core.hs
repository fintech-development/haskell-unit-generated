module TheUnit.Model.Core where

import qualified Data.Aeson as J
import qualified Data.Text as T
import Prelude as P

-- | Removes Null fields.  (OpenAPI-Specification 2.0 does not allow Null in JSON)
_omitNulls :: [(T.Text, J.Value)] -> J.Value
_omitNulls = J.object . P.filter notNull
  where
    notNull (_, J.Null) = False
    notNull _ = True
