{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bot.Data.CalendarEntry where

import Bot.Data.AtomFeed (getContentAsText, parseFeedDate)
import Bot.Data.Gregorian (Gregorian (Gregorian))
import Control.Applicative ((<|>))
import Data.List (find, uncons)
import Data.Text (Text)
import Data.Time (LocalTime (localDay), toGregorian)
import qualified Text.Atom.Feed as Atom
import Text.Mustache ((~>))
import qualified Text.Mustache as Mstch
import Data.Maybe (isNothing)

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
        _entrySummary = entrySummary,
        _entryContent = getEntryContentAsText =<< Atom.entryContent e,
        _entryPublished
      }
  where
    entryUrl :: Atom.Entry -> Maybe Text
    entryUrl e = Atom.linkHref <$> findAlternate (Atom.entryLinks e)
      where
        findAlternate = find (\Atom.Link {linkRel} -> linkRel == Just (Right "alternate"))
    entrySummary :: Maybe Text
    entrySummary =
      let rawSummary = Atom.entrySummary e
          rawContent = Atom.entryContent e
       in (getContentAsText <$> rawSummary) <|> (contentAsSummary =<< rawContent)
    contentAsSummary :: Atom.EntryContent -> Maybe Text
    contentAsSummary ec = case ec of
      Atom.TextContent txt -> Just txt -- 外部記事のとき
      _ -> Nothing
    getEntryContentAsText :: Atom.EntryContent -> Maybe Text
    getEntryContentAsText ec = case ec of
      Atom.HTMLContent txt -> Just txt
      _ -> Nothing -- 面倒なのでなかったことにする

isSummaryEmpty :: CalendarEntry -> Bool
isSummaryEmpty CalendarEntry {_entrySummary} = isNothing _entrySummary

fillSummary :: (Applicative f) => (Text -> f Text) -> CalendarEntry -> f CalendarEntry
fillSummary summarizer en@CalendarEntry {_entryContent} =
  case (isSummaryEmpty en, _entryContent) of
    (True, Just ec) -> (\s -> en {_entrySummary = Just s}) <$> summarizer ec
    _ -> pure en
