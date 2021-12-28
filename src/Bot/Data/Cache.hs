module Bot.Data.Cache where

import Bot.Data.AtomFeed (parseFeedDate)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (LocalTime)

type Cache = Map String LocalTime

updateCache :: String -> Text -> Cache -> Cache
updateCache url updated = Map.alter (const (parseFeedDate updated)) url
