{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bot.AppM (AppM, runAppM, (<?))
import Bot.Capability.Cache (readCache, writeCache)
import Bot.Capability.FetchFeed (fetchFeed)
import Bot.Capability.PostMsg (postMsg)
import Bot.Capability.RenderMsg (render)
import Bot.Capability.Summarize (summarize)
import Bot.Data.AdventCalendar (AdventCalendar (..), forEntries, fromFeed, nullEntries, pickupNewEntryAfter)
import Bot.Data.Args (Args (..))
import Bot.Data.Cache (updateCache)
import Bot.Data.CalendarEntry (fillSummary)
import Control.Applicative (optional, (<|>))
import Control.Monad.Reader (ask)
import Data.Map.Strict ((!?))
import Data.Text (Text)
import qualified Data.Text.IO as Txt (putStrLn)
import Data.Time (LocalTime)
import qualified Data.Version as Ver (showVersion)
import Options.Applicative
  ( Parser,
    ParserInfo,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    hidden,
    info,
    infoOption,
    long,
    metavar,
    short,
    showDefault,
    strArgument,
    strOption,
    switch,
    value,
    (<**>),
  )
import qualified Paths_advent_calendar_bot as Paths
import qualified System.Exit as System (exitFailure, exitSuccess)
import Text.Feed.Types (Feed)

cliArgs :: ParserInfo Args
cliArgs =
  info (opts <**> showVersion <**> helper) $
    fullDesc
      <> header "Command to post Qiita Advent Calendar updates to Mattermost"
  where
    showVersion :: Parser (a -> a)
    showVersion =
      infoOption
        (Ver.showVersion Paths.version)
        ( long "version"
            <> short 'v'
            <> help "Show version"
            <> hidden
        )
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
          ( long "insecure"
              <> short 'k'
              <> showDefault
              <> help "Allow insecure server connections when using SSL"
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
    Left err -> Txt.putStrLn err >> System.exitFailure
    Right out -> Txt.putStrLn out >> System.exitSuccess
  where
    app :: AppM Text
    app = do
      Args {feedUri, templateFilePath, dryRun} <- ask
      cached <- readCache
      feed <- fetchFeed feedUri
      advClndr <- mkAdventCalendor (cached !? feedUri) feed
      msg <- render templateFilePath advClndr
      if dryRun
        then return $ unlessNullEntries advClndr msg
        else do
          ret <- unlessNullEntries advClndr <$> postMsg msg
          writeCache $ updateCache feedUri (_calendarUpdate advClndr) cached
          return ret

    mkAdventCalendor :: Maybe LocalTime -> Feed -> AppM AdventCalendar
    mkAdventCalendor cached feed = do
      mAdvClndr <- fromFeed feed <? ("failed to parse feed" :: Text)
      let advClndr = maybe id pickupNewEntryAfter cached mAdvClndr
      forEntries (fillSummary summarize) advClndr <|> return advClndr

    unlessNullEntries :: AdventCalendar -> Text -> Text
    unlessNullEntries advClndr ret =
      if nullEntries advClndr
        then "There are no new articles."
        else ret
