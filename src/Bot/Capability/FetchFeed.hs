module Bot.Capability.FetchFeed where

import Text.Feed.Types (Feed)

class (Monad m) => FetchFeed m where
  fetchFeed :: String -> m Feed
