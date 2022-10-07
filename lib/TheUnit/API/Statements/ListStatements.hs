module TheUnit.API.Statements.ListStatements (listStatements, listAccountStatements) where

import qualified Data.Proxy as P
import Network.Integrated.HTTP.Auth (Auth20BearerToken)
import Network.Integrated.HTTP.Core (HasOptionalParam)
import qualified Network.Integrated.HTTP.Core as Core
import Network.Integrated.HTTP.MimeTypes (MimeVndApiJSON, Produces)
import TheUnit.Model.Relationships.AccountId (AccountId (getAccountId))
import TheUnit.Model.Response (UnitResponse)
import TheUnit.Model.Statements (Statement)

data ListStatementsRequest

instance Produces ListStatementsRequest MimeVndApiJSON

instance HasOptionalParam ListStatementsRequest FilterAccountId where
  applyOptionalParam req (FilterAccountId accountId) =
    req `Core.addQuery` Core.toQuery ("filter[accountId]", Just $ getAccountId accountId)

newtype FilterAccountId = FilterAccountId AccountId

-- * Operations

-- *** List statement resources. Filtering and paging can be applied.

-- | @POST \/statements@
--
-- AuthMethod: 'Auth20BearerToken'
--
-- #Query Parameters#
-- - `page[limit]`	`integer`	default = 100	Optional.
-- Maximum number of resources that will be returned. Maximum is 1000 resources. See Pagination.
-- - `page[offset]`	`integer`	default = 0	Optional.
-- Number of resources to skip. See Pagination.
-- - `filter[accountId]`	`string`	default = (empty)	Optional.
-- Filters the results by the specified account id.
-- `filter[customerId]`	`string`	default=(empty)	Optional.
-- Filters the results by the specified customer id.
-- - `filter[period]`	`ISO8601 Date string`	default=(empty)	Optional.
-- Filters the results for a specific month. e.g. 2021-01
-- - `sort`	`string`	sort=period	Optional.
-- Leave empty or provide sort=period for ascending order. Provide sort=-period (leading minus sign) for descending order.
-- TODO @asimuskov : add more query params
-- TODO @asimuskov : add pagination support in response
listStatements ::
  Core.Request ListStatementsRequest MimeVndApiJSON (UnitResponse [Statement]) MimeVndApiJSON
listStatements =
  Core._mkRequest "GET" ["/statements"]
    `Core._hasAuthType` (P.Proxy :: P.Proxy Auth20BearerToken)

listAccountStatements ::
  AccountId ->
  Core.Request ListStatementsRequest MimeVndApiJSON (UnitResponse [Statement]) MimeVndApiJSON
listAccountStatements accountId =
  listStatements `Core.applyOptionalParam` FilterAccountId accountId
