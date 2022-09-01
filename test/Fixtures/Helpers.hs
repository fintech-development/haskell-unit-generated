module Fixtures.Helpers where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.UUID.V4 (nextRandom)

fromLeftError :: String -> Either String a -> a
fromLeftError prefix = \case
  Left e -> error $ prefix <> ": " <> e
  Right r -> r

fromMaybeError :: String -> Maybe a -> a
fromMaybeError errorText = fromMaybe (error errorText)

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

uuidV4 :: IO T.Text
uuidV4 = tshow <$> nextRandom
