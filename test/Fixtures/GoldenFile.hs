module Fixtures.GoldenFile
  ( GoldenFile (..),
    IndividualApplicationFiles (..),
    goldenFile,
  )
where

import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate)
import qualified Data.Text as T

data GoldenFile
  = IndividualApplication IndividualApplicationFiles

instance Show GoldenFile where
  show (IndividualApplication file) = intercalate "/" ["individualApplication", show file]

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

goldenFilePath :: T.Text
goldenFilePath = T.intercalate "/" ["./test", "Fixtures", "goldenfiles"]

goldenFile :: GoldenFile -> IO BL.ByteString
goldenFile file =
  let filePath = T.pack $ show file
      goldenPath = T.intercalate "/" [goldenFilePath, filePath]
   in BL.readFile $ T.unpack $ goldenPath
