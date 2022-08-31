import qualified Aeson
import Test.Hspec (hspec)

main :: IO ()
main =
  hspec Aeson.spec
