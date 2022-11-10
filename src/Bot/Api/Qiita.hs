module Bot.Api.Qiita where

import Bot.Api.Util (ReqInfo, reqGet, useStr)
import qualified Bot.Util as Util
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Text (Text)
import qualified Network.HTTP.Req as Req
import qualified Text.Feed.Import as Import
import Text.Feed.Types (Feed)

getCalendarFeed :: String -> IO (Either Text Feed)
getCalendarFeed feedUri = runExceptT $ do
  eUrlInfo <- Util.hoistMaybe "Invalid API URL" (useStr feedUri)
  either fetch fetch eUrlInfo
  where
    fetch :: ReqInfo scheme -> ExceptT Text IO Feed
    fetch info = do
      let req = Req.responseBody <$> reqGet info Req.lbsResponse
      res <- Req.runReq Req.defaultHttpConfig req
      Util.hoistMaybe "Parse RSS Feed Faild" $ Import.parseFeedSource res
