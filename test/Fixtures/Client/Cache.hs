module Fixtures.Client.Cache where

import Control.Concurrent.STM (TBQueue, atomically, newTBQueueIO, tryReadTBQueue, writeTBQueue)
import GHC.Generics (Generic)
import TheUnit.Model.Relationships (AccountId, CustomerId)

data Cache = Cache
  { customers :: TBQueue CustomerId,
    accounts :: TBQueue AccountId
  }
  deriving (Generic)

initCache :: IO Cache
initCache = do
  customersCache <- newTBQueueIO 10
  accountsCache <- newTBQueueIO 10
  pure Cache {customers = customersCache, accounts = accountsCache}

get :: (cache -> TBQueue a) -> cache -> IO (Maybe a)
get f = atomically . tryReadTBQueue . f

put :: (cache -> TBQueue a) -> cache -> a -> IO ()
put f cache = atomically . writeTBQueue (f cache)

getCustomer :: Cache -> IO (Maybe CustomerId)
getCustomer = get customers

putCustomer :: Cache -> CustomerId -> IO ()
putCustomer = put customers

getAccount :: Cache -> IO (Maybe AccountId)
getAccount = get accounts

putAccount :: Cache -> AccountId -> IO ()
putAccount = put accounts
