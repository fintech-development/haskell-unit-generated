module TheUnit.Model.Relationships.PaymentId (PaymentId (..), BulkPaymentsId (..), ReceivedPaymentId (..)) where

import qualified Data.Aeson as J
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import TheUnit.Model.Relationships.RelationshipsObject (RelationshipsObject (..))

-- | PaymentId relationships id
newtype PaymentId = PaymentId {getPaymentId :: T.Text}
  deriving (Show, Eq, Generic)
  deriving (OpenApi.ToSchema) via T.Text

instance J.ToJSON PaymentId where
  toJSON (PaymentId _id) =
    J.toJSON $ RelationshipsObject "payment" _id

instance J.FromJSON PaymentId where
  parseJSON o = do
    RelationshipsObject {..} <- J.parseJSON o
    case _type of
      "payment" -> pure ()
      _ -> fail "not payment"
    pure $ PaymentId _id

-- | BulkPaymentsId relationships id
newtype BulkPaymentsId = BulkPaymentsId {getBulkPaymentsId :: T.Text}
  deriving (Show, Eq, Generic)
  deriving (OpenApi.ToSchema) via T.Text

instance J.ToJSON BulkPaymentsId where
  toJSON (BulkPaymentsId _id) =
    J.toJSON $ RelationshipsObject "bulkPayments" _id

instance J.FromJSON BulkPaymentsId where
  parseJSON o = do
    RelationshipsObject {..} <- J.parseJSON o
    case _type of
      "bulkPayments" -> pure ()
      _ -> fail "not bulkPayments"
    pure $ BulkPaymentsId _id

-- | ReceivedPaymentId relationships id
newtype ReceivedPaymentId = ReceivedPaymentId {getReceivedPaymentId :: T.Text}
  deriving (Show, Eq, Generic)
  deriving (OpenApi.ToSchema) via T.Text

instance J.ToJSON ReceivedPaymentId where
  toJSON (ReceivedPaymentId _id) =
    J.toJSON $ RelationshipsObject "receivedPayment" _id

instance J.FromJSON ReceivedPaymentId where
  parseJSON o = do
    RelationshipsObject {..} <- J.parseJSON o
    case _type of
      "receivedPayment" -> pure ()
      _ -> fail "not receivedPayment"
    pure $ ReceivedPaymentId _id
