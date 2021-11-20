{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((^.))
import Data.Text (Text)
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt
import qualified Network.Wreq as Wreq
import qualified Network.Mattermost.Endpoints as Mttmst
import System.Environment (getArgs)
import qualified Text.Atom.Feed as Atom
import qualified Text.Feed.Import as Import (parseFeedSource)
import Text.Feed.Types (Feed (AtomFeed))

main :: IO ()
main = do
  [uri] <- getArgs
  res <- Wreq.get uri
  mapM_ Txt.putStr $ renderFeed <$> Import.parseFeedSource (res ^. Wreq.responseBody)

renderFeed :: Feed -> Text
renderFeed feed = case feed of
  AtomFeed f -> renderAsMarkdown $ getEntryTitleAsText <$> Atom.feedEntries f
  _ -> Txt.empty
  where
    getEntryTitleAsText :: Atom.Entry -> Text
    getEntryTitleAsText entry = case Atom.entryTitle entry of
      Atom.TextString t -> t
      Atom.HTMLString t -> t
      t@(Atom.XHTMLString _) -> Txt.pack (Atom.txtToString t)

    renderAsMarkdown :: [Text] -> Text
    renderAsMarkdown = Txt.unlines . fmap (Txt.append "* ")
