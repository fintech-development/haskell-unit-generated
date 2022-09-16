module TheUnit.Model.Webhooks.Verify
  ( pattern UnitWebhookSignatureHeader,
  )
where

import qualified Data.Text as T

pattern UnitWebhookSignatureHeader :: T.Text
pattern UnitWebhookSignatureHeader = "x-unit-signature"
