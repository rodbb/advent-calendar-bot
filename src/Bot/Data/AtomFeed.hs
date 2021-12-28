module Bot.Data.AtomFeed where

import Data.Text (Text)
import qualified Data.Text as Txt
import Data.Time (ParseTime, defaultTimeLocale, parseTimeM)
import qualified Text.Atom.Feed as Atom

parseFeedDate :: (MonadFail m, ParseTime t) => Text -> m t
parseFeedDate = parseTimeM True defaultTimeLocale "%FT%T%EZ" . Txt.unpack

getContentAsText :: Atom.TextContent -> Text
getContentAsText cntnt = case cntnt of
  Atom.TextString t -> t
  Atom.HTMLString t -> t
  t@(Atom.XHTMLString _) -> Txt.pack (Atom.txtToString t)
