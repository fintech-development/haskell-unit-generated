{-# LANGUAGE AllowAmbiguousTypes #-}

module Fixtures.VerifyJSON (verifyJSON) where

import qualified Data.Aeson as J
import Fixtures.GoldenFile (GoldenFile, goldenFile)
import Fixtures.Helpers (fromLeftError, fromMaybeError)
import Test.Hspec.Expectations.Json (shouldMatchJson)

verifyJSON :: forall testValue. (J.ToJSON testValue, J.FromJSON testValue) => GoldenFile -> IO ()
verifyJSON file = do
  rawJsonData <- goldenFile file

  let expected = fromMaybeError "invalid expected" $ J.decode' rawJsonData :: J.Value
  let res = J.eitherDecode' rawJsonData :: Either String testValue
  let req = fromLeftError "JsonDecode" res
  J.toJSON req `shouldMatchJson` expected
