module Bot.Capability.Cache where

import Bot.Data.Cache (Cache)

class (Monad m) => ManageCache m where
  readCache :: m Cache
  writeCache :: Cache -> m ()
