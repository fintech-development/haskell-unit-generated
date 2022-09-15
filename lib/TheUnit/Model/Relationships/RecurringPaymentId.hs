module TheUnit.Model.Relationships.RecurringPaymentId (RecurringPaymentId (..)) where

import qualified Data.Aeson as J
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import TheUnit.Model.Relationships.RelationshipsObject (RelationshipsObject (..))

-- | RecurringPaymentId relationships id
-- "type" : "recurringPayment"
newtype RecurringPaymentId = RecurringPaymentId {getRecurringPaymentId :: T.Text}
  deriving (Show, Eq, Generic)
  deriving (OpenApi.ToSchema) via T.Text

instance J.ToJSON RecurringPaymentId where
  toJSON (RecurringPaymentId _id) =
    J.toJSON $ RelationshipsObject "recurringPayment" _id

instance J.FromJSON RecurringPaymentId where
  parseJSON o = do
    RelationshipsObject {..} <- J.parseJSON o
    case _type of
      -- In Payments relationship
      "recurringPayment" -> pure ()
      "recurringCreditAchPayment" -> pure ()
      "recurringCreditBookPayment" -> pure ()
      _ -> fail "not recurringPayment"
    pure $ RecurringPaymentId _id
