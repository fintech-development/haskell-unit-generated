module TheUnit.Model.Statements (Statement (..)) where

import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as J
import qualified Data.Text as T
import GHC.Generics (Generic)
import TheUnit.Model.Core (_omitNulls)
import TheUnit.Model.Relationships (AccountId, CustomerId)

data Statement = Statement
  { -- | Identifier of the statement resource.
    id :: !T.Text,
    -- | Period of the statement, formatted YYYY-MM, e.g "2020-05".
    period :: !T.Text,
    --

    -- | The Customer the deposit account belongs to. This relationship is only available if the account belongs to a single customer, business or individual.
    customer :: !(Maybe CustomerId),
    -- | The account for which the statement was produced.
    accountId :: !AccountId
  }
  deriving (Show, Generic)

instance Eq Statement where
  Statement {id = idL} == Statement {id = idR} = idL == idR

instance J.FromJSON Statement where
  parseJSON = J.withObject "Statement" \o -> do
    _type :: T.Text <- o .: "type"
    case _type of
      "statement" -> pure () -- from documentation
      "accountStatementDTO" -> pure () -- from postman and SandboxAPI
      _ -> fail $ "Cannot parse " <> T.unpack _type <> "as statement"
    attributes <- o .: "attributes"
    relationships <- o .: "relationships"

    _id <- o .: "id"
    period <- attributes .: "period"
    customer <- relationships .:? "customer"
    accountId <- relationships .: "account"
    pure Statement {id = _id, ..}

instance J.ToJSON Statement where
  toJSON Statement {..} =
    let attributes =
          _omitNulls
            [ "period" .= period
            ]
        relationships =
          _omitNulls
            [ "customer" .= J.toJSON customer,
              "account" .= J.toJSON accountId
            ]
     in _omitNulls
          [ "type" .= ("statement" :: T.Text),
            "id" .= id,
            "attributes" .= attributes,
            "relationships" .= relationships
          ]
