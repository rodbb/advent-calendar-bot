{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative (Alternative, optional, (<|>))
import Control.Exception (catch, throwIO)
import Control.Lens ((&), (.~), (^.), (^?))
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT), exceptToMaybeT)
import Data.Aeson
  ( FromJSON,
    ToJSON,
    defaultOptions,
    genericToEncoding,
    toEncoding,
    toJSON,
  )
import qualified Data.ByteString.Char8 as B (pack)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL (putStr)
import Data.Foldable (Foldable (fold))
import Data.List (find, uncons)
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Txt (pack, unpack)
import qualified Data.Text.IO as Txt (putStr, writeFile)
import Data.Time
  ( LocalTime (localDay),
    ParseTime,
    defaultTimeLocale,
    parseTimeM,
    toGregorian,
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
    switch,
    value,
    (<**>),
  )
import qualified Paths_advent_calendar_bot as Paths (getDataDir)
import System.IO.Error (isDoesNotExistError)
import qualified Text.Atom.Feed as Atom
import qualified Text.Feed.Import as Import (parseFeedSource)
import Text.Feed.Types (Feed (AtomFeed))
import Text.Mustache ((~>))
import qualified Text.Mustache as Mstch
import Text.URI (mkURI)
import qualified Text.URI as URI (relativeTo, render)

data Args = Args
  { mttrWebhookUrl :: String,
    feedUri :: String,
    summaryApiUrl :: Maybe String,
    summaryApiKey :: Maybe String,
    templateFilePath :: FilePath,
    cachePath :: FilePath,
    dryRun :: Bool
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
  ret <- go =<< execParser cliArgs
  case ret of
    Nothing -> Txt.putStr "Something Wrong!"
    Just x0 -> BL.putStr x0
  putStrLn ""
  where
    go Args {..} =
      runMaybeT $ do
        cached <- liftIO (readCache cachePath)
        feed <- MaybeT (getCalendarFeed feedUri)
        advClndr <- MaybeT . return $
          case cached !? feedUri of
            Nothing -> fromFeed feed
            Just updTime -> pickupNewEntryAfter updTime <$> fromFeed feed
        advClndr' <- case summaryApiUrl of
          Just url -> summarizeEntryContent url summaryApiKey advClndr
          Nothing -> return advClndr
        msg <- render templateFilePath advClndr'
        if dryRun
          then "" <$ liftIO (Txt.putStr msg)
          else do
            ret <- MaybeT (postToMattermost mttrWebhookUrl msg)
            liftIO $ updateCache feedUri (_calendarUpdate advClndr) cached cachePath
            return ret

readCache :: FilePath -> IO (Map String LocalTime)
readCache f =
  (read <$> readFile f) `catch` \e -> do
    if isDoesNotExistError e
      then return mempty
      else throwIO e

updateCache :: String -> Text -> Map String LocalTime -> FilePath -> IO ()
updateCache url updated cache path =
  let newCache = Map.alter (const (parseFeedDate updated)) url cache
   in Txt.writeFile path (Txt.pack $ show newCache)

getCalendarFeed :: String -> IO (Maybe Feed)
getCalendarFeed feedUri = do
  res <- Wreq.get feedUri
  return $ Import.parseFeedSource (res ^. Wreq.responseBody)

parseFeedDate :: (MonadFail m, Alternative m, ParseTime t) => Text -> m t
parseFeedDate txt =
  let str = Txt.unpack txt
   in parseAs "%FT%T%Ez" str -- format until 2020
        <|> parseAs "%FT%T%Z" str -- format from 2021
  where
    parseAs = parseTimeM True defaultTimeLocale

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
    _entryContent :: Maybe Text,
    _entryPublished :: LocalTime
  }
  deriving (Show)

instance Mstch.ToMustache CalendarEntry where
  toMustache CalendarEntry {..} =
    Mstch.object
      [ "entryTitle" ~> _entryTitle,
        "entryAuthor" ~> _entryAuthor,
        "entryUrl" ~> _entryUrl,
        "entrySummary" ~> _entrySummary,
        "entryPublished" ~> Gregorian (toGregorian $ localDay _entryPublished)
      ]

newtype Gregorian = Gregorian (Integer, Int, Int)
  deriving (Show)

instance Mstch.ToMustache Gregorian where
  toMustache (Gregorian (y, m, d)) =
    Mstch.object
      [ "year" ~> y,
        "month" ~> m,
        "day" ~> d
      ]

pickupNewEntryAfter :: LocalTime -> AdventCalendar -> AdventCalendar
pickupNewEntryAfter mt ac@AdventCalendar {_calendarEntries} =
  ac {_calendarEntries = filter isNewEntry _calendarEntries}
  where
    isNewEntry :: CalendarEntry -> Bool
    isNewEntry CalendarEntry {_entryPublished} = _entryPublished > mt

render :: FilePath -> AdventCalendar -> MaybeT IO Text
render templatePath viewModel = do
  dataDir <- liftIO Paths.getDataDir
  template <- exceptToMaybeT $ ExceptT $ Mstch.automaticCompile [".", dataDir] templatePath
  return (Mstch.substitute template viewModel)

summarizeEntryContent :: String -> Maybe String -> AdventCalendar -> MaybeT IO AdventCalendar
summarizeEntryContent url mApiKey ac@AdventCalendar {_calendarEntries} = do
  entries <- traverse getSummary _calendarEntries
  return ac {_calendarEntries = entries}
  where
    getSummary :: CalendarEntry -> MaybeT IO CalendarEntry
    getSummary en@CalendarEntry {_entrySummary, _entryContent} = do
      reqBody <- MaybeT . return $ (`ApiRequestBody` 1000) <$> _entryContent
      let opts = case mApiKey of
            Just apiKey -> Wreq.defaults & Wreq.header "x-api-key" .~ [B.pack apiKey]
            Nothing -> Wreq.defaults
      res <- liftIO $ Wreq.asJSON =<< Wreq.postWith opts url (toJSON reqBody)
      let mRet = res ^? Wreq.responseBody
      return en {_entrySummary = fold . result <$> mRet}

data ApiRequestBody = ApiRequestBody
  { text :: Text,
    length :: Int
  }
  deriving (Generic)

instance ToJSON ApiRequestBody where
  toEncoding = genericToEncoding defaultOptions

newtype ApiResponseBody = ApiResponseBody {result :: [Text]}
  deriving (Generic)

instance FromJSON ApiResponseBody

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
            _entrySummary = getContentAsText <$> Atom.entrySummary e,
            _entryContent = getEntryContentAsText =<< Atom.entryContent e,
            _entryPublished
          }
      where
        getEntryContentAsText :: Atom.EntryContent -> Maybe Text
        getEntryContentAsText ec = case ec of
          Atom.TextContent txt -> Just txt
          Atom.HTMLContent txt -> Just txt
          _ -> Nothing -- 面倒なのでなかったことにする
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
