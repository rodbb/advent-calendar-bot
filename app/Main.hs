{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception (catch, throwIO)
import Control.Lens ((^.), (^?))
import Data.Aeson
  ( ToJSON,
    defaultOptions,
    genericToEncoding,
    toEncoding,
    toJSON,
  )
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as Txt (unpack, unlines, pack, append)
import qualified Data.Text.IO as Txt (readFile, writeFile)
import Data.Time
  ( ParseTime,
    defaultTimeLocale,
    parseTimeM,
  )
import GHC.Generics (Generic)
import qualified Network.Wreq as Wreq
import Options.Applicative
  ( Parser,
    ParserInfo,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    metavar,
    strArgument,
    (<**>),
  )
import System.IO.Error (isDoesNotExistError)
import qualified Text.Atom.Feed as Atom
import qualified Text.Feed.Import as Import (parseFeedSource)
import qualified Text.Feed.Query as Feed
import Text.Feed.Types (Feed (AtomFeed))

data Args = Args
  { mttrWebhookUrl :: String,
    feedUri :: String
  }

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

cacheFileName :: String
cacheFileName = ".advent-calandar-bot"

main :: IO ()
main = do
  cache <- readCache cacheFileName
  Args {..} <- execParser cliArgs
  feed <- getCalendarFeed feedUri
  ret <- mapM_ (postToMattermost mttrWebhookUrl) (renderFeed =<< feed)
  mapM_ (Txt.writeFile cacheFileName) $ Feed.getFeedLastUpdate =<< feed
  print ret

readCache :: FilePath -> IO (Maybe Text)
readCache f =
  (Just <$> Txt.readFile f) `catch` \e -> do
    if isDoesNotExistError e
      then return Nothing
      else throwIO e

getCalendarFeed :: String -> IO (Maybe Feed)
getCalendarFeed feedUri = do
  res <- Wreq.get feedUri
  return $ Import.parseFeedSource (res ^. Wreq.responseBody)

parseFeedDate :: (MonadFail m, ParseTime t) => String -> m t
parseFeedDate = parseTimeM False defaultTimeLocale "%FT%T%Ez"

renderFeed :: Feed -> Maybe Text
renderFeed feed = case feed of
  AtomFeed f -> Just $ renderAsMarkdown $ getEntryTitleAsText <$> Atom.feedEntries f
  _ -> Nothing
  where
    getEntryTitleAsText :: Atom.Entry -> Text
    getEntryTitleAsText entry = case Atom.entryTitle entry of
      Atom.TextString t -> t
      Atom.HTMLString t -> t
      t@(Atom.XHTMLString _) -> Txt.pack (Atom.txtToString t)

    renderAsMarkdown :: [Text] -> Text
    renderAsMarkdown = Txt.unlines . fmap (Txt.append "* ")

type WebhookUrl = String

postToMattermost :: WebhookUrl -> Text -> IO (Maybe ByteString)
postToMattermost webhookUrl theMsg = do
  res <- Wreq.post webhookUrl $ toJSON MttrmstMsg {text = theMsg}
  return (res ^? Wreq.responseBody)

newtype MttrmstMsg = MttrmstMsg {text :: Text}
  deriving (Generic)

instance ToJSON MttrmstMsg where
  toEncoding = genericToEncoding defaultOptions
