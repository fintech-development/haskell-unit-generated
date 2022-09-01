import qualified Aeson
import qualified Integration
import Test.Hspec (hspec)

main :: IO ()
main =
  hspec do
    Aeson.spec
    Integration.spec
