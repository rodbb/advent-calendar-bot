module Bot.Api.Qiita where

import Control.Lens ((^.))
import qualified Network.Wreq as Wreq
import qualified Text.Feed.Import as Import
import Text.Feed.Types (Feed)

getCalendarFeed :: String -> IO (Maybe Feed)
getCalendarFeed feedUri = do
  res <- Wreq.get feedUri
  return $ Import.parseFeedSource (res ^. Wreq.responseBody)
