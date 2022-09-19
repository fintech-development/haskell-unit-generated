{-# LANGUAGE AllowAmbiguousTypes #-}

module Fixtures.VerifyJSON (verifyJSON, verifyJSON_) where

import Control.Monad (void)
import qualified Data.Aeson as J
import Fixtures.GoldenFile (GoldenFile, goldenFile)
import Fixtures.Helpers (fromLeftError, fromMaybeError)
import Test.Hspec.Expectations.Json (shouldMatchJson)

verifyJSON :: forall testValue. (J.ToJSON testValue, J.FromJSON testValue) => GoldenFile -> IO testValue
verifyJSON file = do
  rawJsonData <- goldenFile file

  let expected = fromMaybeError "invalid expected" $ J.decode' rawJsonData :: J.Value
  let res = J.eitherDecode' rawJsonData :: Either String testValue
  let req = fromLeftError "JsonDecode" res
  J.toJSON req `shouldMatchJson` expected
  pure req

verifyJSON_ :: forall testValue. (J.ToJSON testValue, J.FromJSON testValue) => GoldenFile -> IO ()
verifyJSON_ file =
  void $ verifyJSON @testValue file
