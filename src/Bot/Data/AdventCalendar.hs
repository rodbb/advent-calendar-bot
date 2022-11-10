{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bot.Data.AdventCalendar where

import Bot.Data.AtomFeed (getContentAsText)
import Bot.Data.CalendarEntry (CalendarEntry (..), fromAtomEntry)
import Control.Applicative (Alternative, (<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToEncoding, toJSON)
import Data.Aeson.Types (ToJSON (toEncoding))
import qualified Data.ByteString.Char8 as B (pack)
import Data.Foldable (fold)
import Data.List (find, uncons)
import Data.Text (Text)
import qualified Data.Text as Txt
import Data.Time (LocalTime, ParseTime, defaultTimeLocale, parseTimeM)
import GHC.Generics (Generic)
import qualified Text.Atom.Feed as Atom
import Text.Feed.Types (Feed (AtomFeed))
import Text.Mustache ((~>))
import qualified Text.Mustache as Mstch
import Text.URI (mkURI)
import qualified Text.URI as URI

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

pickupNewEntryAfter :: LocalTime -> AdventCalendar -> AdventCalendar
pickupNewEntryAfter mt ac@AdventCalendar {_calendarEntries} =
  ac {_calendarEntries = filter isNewEntry _calendarEntries}
  where
    isNewEntry :: CalendarEntry -> Bool
    isNewEntry CalendarEntry {_entryPublished} = _entryPublished > mt

nullEntries :: AdventCalendar -> Bool
nullEntries AdventCalendar {_calendarEntries} = null _calendarEntries

forEntries :: (Applicative f) => (CalendarEntry -> f CalendarEntry) -> AdventCalendar -> f AdventCalendar
forEntries func ac@AdventCalendar {_calendarEntries} =
  (\es -> ac {_calendarEntries = es}) <$> traverse func _calendarEntries
