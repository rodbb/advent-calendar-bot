{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bot.AppM (AppM, hoistMaybe, runAppM)
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
import qualified Data.Text.IO as Txt (putStrLn)
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
          ( long "dry-run"
              <> showDefault
              <> help "If true, will not post to Mattermost"
          )

main :: IO ()
main = do
  ret <- runAppM app =<< execParser cliArgs
  case ret of
    Left err -> do
      putStrLn err
      System.exitFailure
    Right x0 -> do
      BL.putStr x0
      putStrLn ""
      System.exitSuccess
  where
    app :: AppM ByteString
    app = do
      Args {feedUri, summaryApiUrl, templateFilePath, dryRun} <- ask
      cached <- readCache
      feed <- fetchFeed feedUri
      let feedClndr = hoistMaybe "Invalid Feed" (fromFeed feed)
      advClndr <- case cached !? feedUri of
        Nothing -> feedClndr
        Just updTime -> pickupNewEntryAfter updTime <$> feedClndr
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
            then liftIO ("" <$ Txt.putStrLn msg)
            else do
              ret <- postMsg msg
              writeCache newCache
              return ret
