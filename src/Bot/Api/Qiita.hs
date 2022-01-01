module Bot.Api.Qiita where

import Bot.Api.Util (ReqInfo, reqGet, useStr)
import qualified Bot.Util as Util
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import qualified Network.HTTP.Req as Req
import qualified Text.Feed.Import as Import
import Text.Feed.Types (Feed)

getCalendarFeed :: String -> IO (Maybe Feed)
getCalendarFeed feedUri = runMaybeT $ do
  eUrlInfo <- Util.hoistMaybe (useStr feedUri)
  either fetch fetch eUrlInfo
  where
    fetch :: ReqInfo scheme -> MaybeT IO Feed
    fetch info = do
      let req = Req.responseBody <$> reqGet info Req.lbsResponse
      res <- Req.runReq Req.defaultHttpConfig req
      Util.hoistMaybe $ Import.parseFeedSource res
