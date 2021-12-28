{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bot.AppM
import Bot.Capability.Cache (readCache, writeCache)
import Bot.Capability.FetchFeed (fetchFeed)
import Bot.Capability.PostMsg (postMsg)
import Bot.Capability.RenderMsg (render)
import Bot.Capability.Summarize (summarize)
import Bot.Data.AdventCalendar (AdventCalendar (..), forEntries, fromFeed, nullEntries, pickupNewEntryAfter)
import Bot.Data.Args (Args (..))
import Bot.Data.Cache (updateCache)
import Bot.Data.CalendarEntry (fillSummary)
import Control.Applicative (optional)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL (putStr)
import Data.Map.Strict ((!?))
import qualified Data.Text.IO as Txt (putStr)
import Options.Applicative
  ( Parser,
    ParserInfo,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    showDefault,
    strArgument,
    strOption,
    switch,
    value,
    (<**>),
  )

cliArgs :: ParserInfo Args
cliArgs =
  info (opts <**> helper) $
    fullDesc
      <> header "Command to post Qiita Advent Calendar updates to Mattermost"
  where
    opts :: Parser Args
    opts =
      Args
        <$> strArgument
          ( metavar "WEBHOOK_URL"
              <> help "Target Mattermost Incoming Webhook URL"
          )
        <*> strArgument
          ( metavar "FEED_URL"
              <> help "Target Qiita Advent Calendar Feed URL"
          )
        <*> optional
          ( strOption
              ( long "summary-api"
                  <> metavar "URL"
                  <> help "Generate Summary API URL"
              )
          )
        <*> optional
          ( strOption
              ( long "summary-api-key"
                  <> metavar "STRING"
                  <> help "Generate Summary API Key String"
              )
          )
        <*> strOption
          ( long "template"
              <> metavar "FILE"
              <> value "templates/default.mustache.md"
              <> showDefault
              <> help "Message Template File Path"
          )
        <*> strOption
          ( long "cache"
              <> metavar "FILE"
              <> value ".advent-calendar-bot"
              <> showDefault
              <> help "Feed Updated Date Cache File Path"
          )
        <*> switch
          ( long "dry-run"
              <> showDefault
              <> help "If true, will not post to Mattermost"
          )

main :: IO ()
main = do
  ret <- runAppM app =<< execParser cliArgs
  case ret of
    Nothing -> Txt.putStr "Something Wrong!"
    Just x0 -> BL.putStr x0
  putStrLn ""
  where
    app :: AppM ByteString
    app = do
      Args {feedUri, summaryApiUrl, templateFilePath, dryRun} <- ask
      cached <- readCache
      feed <- fetchFeed feedUri
      advClndr <- hoistMaybe $ case cached !? feedUri of
        Nothing -> fromFeed =<< feed
        Just updTime -> pickupNewEntryAfter updTime <$> (fromFeed =<< feed)
      let newCache = updateCache feedUri (_calendarUpdate advClndr) cached
      if nullEntries advClndr
        then do
          writeCache newCache
          return "There are no new articles."
        else do
          advClndr' <- case summaryApiUrl of
            Just _ -> forEntries (fillSummary summarize) advClndr
            Nothing -> return advClndr
          msg <- render templateFilePath advClndr'
          if dryRun
            then liftIO ("" <$ Txt.putStr msg)
            else do
              ret <- postMsg msg
              writeCache newCache
              return ret
