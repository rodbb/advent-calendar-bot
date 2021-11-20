{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((^.))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as Txt
import qualified Network.Wreq as Wreq
import System.Environment (getArgs)
import qualified Text.Feed.Export as Export (textFeedWith)
import qualified Text.Feed.Import as Import (parseFeedSource)
import Text.Feed.Types (Feed)
import Text.XML (def, rsPretty)

main :: IO ()
main = do
  [uri] <- getArgs
  res <- Wreq.get uri
  mapM_ Txt.putStr $ renderFeed =<< Import.parseFeedSource (res ^. Wreq.responseBody)

renderFeed :: Feed -> Maybe Text
renderFeed = Export.textFeedWith def {rsPretty = True}
