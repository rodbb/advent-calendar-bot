{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative ((<|>))
import Control.Exception (catch, throwIO)
import Control.Lens ((^.), (^?))
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT), exceptToMaybeT)
import Data.Aeson
  ( ToJSON,
    defaultOptions,
    genericToEncoding,
    toEncoding,
    toJSON,
  )
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL (putStr)
import Data.List (find, uncons)
import Data.Text (Text)
import qualified Data.Text as Txt (pack, unpack)
import qualified Data.Text.IO as Txt (putStr, readFile, writeFile)
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
    long,
    metavar,
    showDefault,
    strArgument,
    strOption,
    value,
    (<**>),
  )
import System.IO.Error (isDoesNotExistError)
import qualified Text.Atom.Feed as Atom
import qualified Text.Feed.Import as Import (parseFeedSource)
import qualified Text.Feed.Query as Feed
import Text.Feed.Types (Feed (AtomFeed))
import Text.Mustache ((~>))
import qualified Text.Mustache as Mstch
import Text.URI (mkURI)
import qualified Text.URI as URI (relativeTo, render)

data Args = Args
  { mttrWebhookUrl :: String,
    feedUri :: String,
    templateFilePath :: FilePath
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
        <*> strOption
          ( long "template"
              <> metavar "FILE"
              <> value "templates/default.mustache.md"
              <> showDefault
              <> help "Message Template File Path"
          )

cacheFileName :: String
cacheFileName = ".advent-calandar-bot"

main :: IO ()
main = do
  cache <- readCache cacheFileName
  ret <- go cache =<< execParser cliArgs
  case ret of
    Nothing -> Txt.putStr "Something Wrong!"
    Just x0 -> BL.putStr x0
  where
    go cache Args {..} =
      let cachedDate = parseFeedDate =<< cache
       in runMaybeT $ do
            feed <- MaybeT (getCalendarFeed feedUri)
            msg <- renderFeed cachedDate feed templateFilePath
            ret <- MaybeT (postToMattermost mttrWebhookUrl msg)
            MaybeT $ mapM (Txt.writeFile cacheFileName) $ Feed.getFeedLastUpdate feed
            return ret

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

instance Mstch.ToMustache AdventCalendar where
  toMustache AdventCalendar {..} =
    Mstch.object
      [ "calendarTitle" ~> _calendarTitle,
        "calendarUrl" ~> _calendarUrl,
        "calendarUpdate" ~> _calendarUpdate,
        "calendarEntries" ~> _calendarEntries
      ]

data CalendarEntry = CalendarEntry
  { _entryTitle :: Text,
    _entryAuthor :: Text,
    _entryUrl :: Text,
    _entrySummary :: Maybe Text,
    _entryPublished :: UTCTime
  }
  deriving (Show)

instance Mstch.ToMustache CalendarEntry where
  toMustache CalendarEntry {..} =
    Mstch.object
      [ "entryTitle" ~> _entryTitle,
        "entryAuthor" ~> _entryAuthor,
        "entryUrl" ~> _entryUrl,
        "entrySummary" ~> _entrySummary
      ]

pickupNewEntryAfter :: Maybe UTCTime -> AdventCalendar -> AdventCalendar
pickupNewEntryAfter mt ac@AdventCalendar {_calendarEntries} =
  ac {_calendarEntries = filter isNewEntry _calendarEntries}
  where
    isNewEntry :: CalendarEntry -> Bool
    isNewEntry CalendarEntry {_entryPublished} =
      case mt of
        Nothing -> True
        Just t -> _entryPublished > t

renderFeed :: Maybe UTCTime -> Feed -> FilePath -> MaybeT IO Text
renderFeed mTime feed templatePath = do
  template <- exceptToMaybeT $ ExceptT $ Mstch.localAutomaticCompile templatePath
  viewModel <- MaybeT $ return (pickupNewEntryAfter mTime <$> fromFeed feed)
  return (Mstch.substitute template viewModel)

fromFeed :: Feed -> Maybe AdventCalendar
fromFeed feed = case feed of
  AtomFeed f -> fromAtomFeed f
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
      (_entryAuthor, _) <- uncons $ Atom.personName <$> Atom.entryAuthors e
      _entryUrl <- entryUrl e
      _entryPublished <- (parseFeedDate =<< Atom.entryPublished e) <|> parseFeedDate (Atom.entryUpdated e)
      return
        CalendarEntry
          { _entryTitle = getContentAsText (Atom.entryTitle e),
            _entryAuthor,
            _entryUrl,
            _entrySummary = Nothing,
            _entryPublished
          }

    entryUrl :: Atom.Entry -> Maybe Text
    entryUrl e = Atom.linkHref <$> findAlternate (Atom.entryLinks e)
      where
        findAlternate = find (\Atom.Link {linkRel} -> linkRel == Just (Right "alternate"))

    getContentAsText :: Atom.TextContent -> Text
    getContentAsText cntnt = case cntnt of
      Atom.TextString t -> t
      Atom.HTMLString t -> t
      t@(Atom.XHTMLString _) -> Txt.pack (Atom.txtToString t)

type WebhookUrl = String

postToMattermost :: WebhookUrl -> Text -> IO (Maybe ByteString)
postToMattermost webhookUrl theMsg = do
  res <- Wreq.post webhookUrl $ toJSON MttrmstMsg {text = theMsg}
  return (res ^? Wreq.responseBody)

newtype MttrmstMsg = MttrmstMsg {text :: Text}
  deriving (Generic)

instance ToJSON MttrmstMsg where
  toEncoding = genericToEncoding defaultOptions
