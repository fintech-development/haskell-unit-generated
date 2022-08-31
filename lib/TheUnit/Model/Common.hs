{-# LANGUAGE DeriveAnyClass #-}

module TheUnit.Model.Common where

import qualified Data.Aeson as J
import qualified Data.OpenApi as OpenApi
import GHC.Generics (Generic)

-- SEE: more generated types here:
-- - https://github.com/unit-finance/unit-node-sdk/blob/main/types/common.ts

data BaseListParams = BaseListParams
  { -- | Maximum number of resources that will be returned. Maximum is 1000 resources. See Pagination.
    -- default: 100
    limit :: !(Maybe Int),
    -- | Number of resources to skip. See Pagination.
    -- default: 0
    offset :: !(Maybe Int)
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

-- | JSON object that contains pagination data
newtype Meta = Meta {meta :: MetaPaginationObject}
  deriving (Show, Eq, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

newtype MetaPaginationObject = MetaPaginationObject {pagination :: MetaPagination}
  deriving (Show, Eq, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)

data MetaPagination = MetaPagination
  { total :: Int,
    limit :: Int,
    offset :: Int
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON, OpenApi.ToSchema)
