module Fixtures.GoldenFile
  ( GoldenFile (..),
    IndividualApplicationFiles (..),
    DepositAccountFiles (..),
    PaymentsFiles (..),
    WebhookFiles (..),
    StatementsFiles (..),
    goldenFile,
    goldenIndividualApplicationRequest,
  )
where

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import Data.Generics.Labels ()
import Data.List (intercalate)
import qualified Data.Text as T
import Fixtures.Helpers (uuidV4)
import Lens.Micro ((&), (.~))
import qualified TheUnit.Model as Model

data GoldenFile
  = IndividualApplication IndividualApplicationFiles
  | DepositAccount DepositAccountFiles
  | Payments PaymentsFiles
  | Webhooks WebhookFiles
  | Statements StatementsFiles

instance Show GoldenFile where
  show (IndividualApplication file) = intercalate "/" ["individualApplication", show file]
  show (DepositAccount file) = intercalate "/" ["depositAccount", show file]
  show (Payments file) = intercalate "/" ["payment", show file]
  show (Webhooks file) = intercalate "/" ["webhooks", show file]
  show (Statements file) = intercalate "/" ["statements", show file]

data IndividualApplicationFiles
  = File'Request
  | File'ResponseApproved
  | File'ResponseAwaitingDocuments
  | File'ResponsePendingReview
  | File'ResponsePending
  | File'ResponseDenied
  | File'ResponseCanceled

instance Show IndividualApplicationFiles where
  show File'Request = "ApplicationRequest.json"
  show File'ResponseApproved = "ApplicationResponse_Approved.json"
  show File'ResponseAwaitingDocuments = "ApplicationResponse_AwaitingDocuments.json"
  show File'ResponsePendingReview = "ApplicationResponse_PendingReview.json"
  show File'ResponsePending = "ApplicationResponse_Pending.json"
  show File'ResponseDenied = "ApplicationResponse_Denied.json"
  show File'ResponseCanceled = "ApplicationResponse_Canceled.json"

data DepositAccountFiles
  = File'CreateDepositAccountData
  | File'DepositAccount

instance Show DepositAccountFiles where
  show File'CreateDepositAccountData = "CreateDepositAccountData.json"
  show File'DepositAccount = "DepositAccount.json"

data PaymentsFiles
  = File'BookPayment
  | File'ACHCreditPayment
  | File'ACHDebitPayment
  | File'CreateInlinePaymentRequest
  | File'CreateLinkedPaymentRequest
  | File'CreateVerifiedPaymentRequest

instance Show PaymentsFiles where
  show File'BookPayment = "BookPayment.json"
  show File'ACHCreditPayment = "ACHCreditPayment.json"
  show File'ACHDebitPayment = "ACHDebitPayment.json"
  show File'CreateInlinePaymentRequest = "CreateInlinePaymentRequest.json"
  show File'CreateLinkedPaymentRequest = "CreateLinkedPaymentRequest.json"
  show File'CreateVerifiedPaymentRequest = "CreateVerifiedPaymentRequest.json"

data WebhookFiles
  = File'AccountCreated
  | File'ApplicationAwaitingDocuments
  | File'ReceivedPaymentCreated

instance Show WebhookFiles where
  show File'AccountCreated = "AccountCreated.json"
  show File'ApplicationAwaitingDocuments = "ApplicationAwaitingDocuments.json"
  show File'ReceivedPaymentCreated = "ReceivedPaymentCreated.json"

data StatementsFiles
  = File'SingleStatementObject
  | File'ListStatements

instance Show StatementsFiles where
  show File'SingleStatementObject = "SingleStatementObject.json"
  show File'ListStatements = "ListStatements.json"

goldenFilePath :: T.Text
goldenFilePath = T.intercalate "/" ["./test", "Fixtures", "goldenfiles"]

goldenFile :: GoldenFile -> IO BL.ByteString
goldenFile file =
  let filePath = T.pack $ show file
      goldenPath = T.intercalate "/" [goldenFilePath, filePath]
   in BL.readFile $ T.unpack goldenPath

goldenIndividualApplicationRequest :: IO Model.CreateIndividualApplicationRequest
goldenIndividualApplicationRequest = do
  Just (Model.UnitEnvelope fc) <- J.decodeFileStrict' "./test/Fixtures/goldenfiles/individualApplication/ApplicationRequest.json"
  newIdempotencyKey <- uuidV4
  pure $ fc & #idempotencyKey .~ newIdempotencyKey
