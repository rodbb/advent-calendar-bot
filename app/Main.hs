{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Lens ((^.))
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Txt
import qualified Network.Mattermost as Mttrmst
  ( defaultConnectionPoolConfig,
    initConnectionData,
  )
import qualified Network.Mattermost.Endpoints as Mttrmst
  ( mmCreatePost,
  )
import qualified Network.Mattermost.Types as Mttrmst
  ( ChannelId (CI),
    ConnectionType (ConnectHTTP),
    Hostname,
    Id (Id),
    Port,
    Session (Session, sessConn, sessTok),
    rawPost,
  )
import qualified Network.Mattermost.Types.Internal as Mttrmst
  ( Token (Token),
  )
import qualified Network.Wreq as Wreq
import Options.Applicative
  ( ArgumentFields,
    Mod,
    OptionFields,
    Parser,
    ParserInfo,
    auto,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    short,
    showDefault,
    strArgument,
    strOption,
    value,
    (<**>),
  )
import qualified Text.Atom.Feed as Atom
import qualified Text.Feed.Import as Import (parseFeedSource)
import Text.Feed.Types (Feed (AtomFeed))

data Args = Args
  { mttrHost :: Text,
    mttrPort :: Int,
    mttrParsonalAccessToken :: String,
    destChannelId :: Text,
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
        <$> textOption
          ( long "mttrHost"
              <> metavar "HOST"
              <> value "localhost"
              <> help "Target Mattermost Host name"
              <> showDefault
          )
        <*> option
          auto
          ( long "mttrPort"
              <> metavar "PORT"
              <> value 8065
              <> help "Target Mattermost listening Port"
              <> showDefault
          )
        <*> strOption
          ( long "token"
              <> short 't'
              <> metavar "TOKEN"
              <> help "Parsonal Access Token for create post to Mattermost"
          )
        <*> textArgument
          ( metavar "CHANNEL_ID"
              <> help "Target Mattermost channel ID to post feed"
          )
        <*> strArgument
          ( metavar "FEED_URL"
              <> help "Target Qiita Advent Calendar Feed URL"
          )

    textOption :: Mod OptionFields String -> Parser Text
    textOption = fmap Txt.pack . strOption

    textArgument :: Mod ArgumentFields String -> Parser Text
    textArgument = fmap Txt.pack . strArgument

main :: IO ()
main = do
  Args {..} <- execParser cliArgs
  res <- Wreq.get feedUri
  let theMsg = renderFeed <$> Import.parseFeedSource (res ^. Wreq.responseBody)
  mapM_ (postToMattermost MttrInfo {..} destChannelId) theMsg

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

type ParsonalAccessToken = String

data MttrInfo = MttrInfo
  { mttrHost :: Mttrmst.Hostname,
    mttrPort :: Mttrmst.Port,
    mttrParsonalAccessToken :: ParsonalAccessToken
  }

type ChannelId = Text

postToMattermost :: MttrInfo -> ChannelId -> Text -> IO ()
postToMattermost MttrInfo {..} channelId theMsg =
  let sessTok = Mttrmst.Token mttrParsonalAccessToken
      chanId = Mttrmst.CI (Mttrmst.Id channelId)
      postObj = Mttrmst.rawPost theMsg chanId
   in do
        sessConn <-
          Mttrmst.initConnectionData
            mttrHost
            mttrPort
            Txt.empty
            Mttrmst.ConnectHTTP
            Mttrmst.defaultConnectionPoolConfig
        void $ Mttrmst.mmCreatePost postObj Mttrmst.Session {..}
