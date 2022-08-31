module Fixtures.Helpers where

import Data.Maybe (fromMaybe)

fromLeftError :: String -> Either String a -> a
fromLeftError prefix = \case
  Left e -> error $ prefix <> ": " <> e
  Right r -> r

fromMaybeError :: String -> Maybe a -> a
fromMaybeError errorText = fromMaybe (error errorText)
