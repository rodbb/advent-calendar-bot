{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative ((<|>))
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
import Data.List (find, uncons)
import Data.Text (Text)
import qualified Data.Text as Txt (pack, unpack)
import qualified Data.Text.IO as Txt (readFile, writeFile)
import Data.Time
  ( ParseTime,
    UTCTime,
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
import Text.URI (mkURI)
import qualified Text.URI as URI (relativeTo, render)

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
  let cachedDate = parseFeedDate =<< cache
  ret <- mapM_ (postToMattermost mttrWebhookUrl) (renderFeed cachedDate =<< feed)
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

parseFeedDate :: (MonadFail m, ParseTime t) => Text -> m t
parseFeedDate = parseTimeM True defaultTimeLocale "%FT%T%Ez" . Txt.unpack

data AdventCalendar = AdventCalendar
  { _calendarTitle :: Text,
    _calendarUrl :: Text,
    _calendarUpdate :: Text,
    _calendarEntries :: [CalendarEntry]
  }
  deriving (Show)

data CalendarEntry = CalendarEntry
  { _entryTitle :: Text,
    _entryAuther :: Text,
    _entryUrl :: Text,
    _entrySummary :: Maybe Text,
    _entryPublished :: UTCTime
  }
  deriving (Show)

renderFeed :: Maybe UTCTime -> Feed -> Maybe Text
renderFeed mTime feed = case feed of
  AtomFeed f -> renderAsMarkdown . pickupNewEntryAfter mTime <$> fromAtomFeed f
  _ -> Nothing
  where
    fromAtomFeed :: Atom.Feed -> Maybe AdventCalendar
    fromAtomFeed f = do
      _calendarUrl <- calendarUrl f
      _calendarEntries <- traverse fromAtomEntry (Atom.feedEntries f)
      return
        AdventCalendar
          { _calendarTitle = getContentAsText (Atom.feedTitle f),
            _calendarUrl,
            _calendarUpdate = Atom.feedUpdated f,
            _calendarEntries
          }

    calendarUrl :: Atom.Feed -> Maybe Text
    calendarUrl f = do
      selfLink <- findSelf (Atom.feedLinks f)
      selfUri <- URI.relativeTo <$> mkURI "." <*> mkURI (Atom.linkHref selfLink)
      URI.render <$> selfUri
      where
        findSelf = find (\Atom.Link {linkRel} -> linkRel == Just (Right "self"))

    fromAtomEntry :: Atom.Entry -> Maybe CalendarEntry
    fromAtomEntry e = do
      (_entryAuther, _) <- uncons $ Atom.personName <$> Atom.entryAuthors e
      _entryUrl <- entryUrl e
      _entryPublished <- (parseFeedDate =<< Atom.entryPublished e) <|> parseFeedDate (Atom.entryUpdated e)
      return
        CalendarEntry
          { _entryTitle = getContentAsText (Atom.entryTitle e),
            _entryAuther,
            _entryUrl,
            _entrySummary = Nothing,
            _entryPublished
          }

    entryUrl :: Atom.Entry -> Maybe Text
    entryUrl e = Atom.linkHref <$> findAlternate (Atom.entryLinks e)
      where
        findAlternate = find (\Atom.Link {linkRel} -> linkRel == Just (Right "alternate"))

    pickupNewEntryAfter :: Maybe UTCTime -> AdventCalendar -> AdventCalendar
    pickupNewEntryAfter mt ac@AdventCalendar {_calendarEntries} =
      ac {_calendarEntries = filter (isNewEntry mt) _calendarEntries}

    isNewEntry :: Maybe UTCTime -> CalendarEntry -> Bool
    isNewEntry mt CalendarEntry {_entryPublished} =
      case mt of
        Nothing -> True
        Just t -> _entryPublished > t

    getContentAsText :: Atom.TextContent -> Text
    getContentAsText cntnt = case cntnt of
      Atom.TextString t -> t
      Atom.HTMLString t -> t
      t@(Atom.XHTMLString _) -> Txt.pack (Atom.txtToString t)

    renderAsMarkdown :: AdventCalendar -> Text
    renderAsMarkdown = Txt.pack . show

type WebhookUrl = String

postToMattermost :: WebhookUrl -> Text -> IO (Maybe ByteString)
postToMattermost webhookUrl theMsg = do
  res <- Wreq.post webhookUrl $ toJSON MttrmstMsg {text = theMsg}
  return (res ^? Wreq.responseBody)

newtype MttrmstMsg = MttrmstMsg {text :: Text}
  deriving (Generic)

instance ToJSON MttrmstMsg where
  toEncoding = genericToEncoding defaultOptions
