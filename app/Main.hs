{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

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
import qualified Data.Text as Txt
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
import qualified Text.Atom.Feed as Atom
import qualified Text.Feed.Import as Import (parseFeedSource)
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

main :: IO ()
main = do
  Args {..} <- execParser cliArgs
  res <- Wreq.get feedUri
  let theMsg = renderFeed <$> Import.parseFeedSource (res ^. Wreq.responseBody)
  ret <- mapM_ (postToMattermost mttrWebhookUrl) theMsg
  print ret

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

type WebhookUrl = String

postToMattermost :: WebhookUrl -> Text -> IO (Maybe ByteString)
postToMattermost webhookUrl theMsg = do
  res <- Wreq.post webhookUrl $ toJSON MttrmstMsg {text = theMsg}
  return (res ^? Wreq.responseBody)

newtype MttrmstMsg = MttrmstMsg {text :: Text}
  deriving (Generic)

instance ToJSON MttrmstMsg where
  toEncoding = genericToEncoding defaultOptions
