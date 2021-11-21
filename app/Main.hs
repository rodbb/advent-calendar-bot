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
import System.Environment (getArgs)
import qualified Text.Atom.Feed as Atom
import qualified Text.Feed.Import as Import (parseFeedSource)
import Text.Feed.Types (Feed (AtomFeed))

main :: IO ()
main = do
  [targetHost, targetPort, mttrParsonalAccessToken, taegetChannelId, feedUri] <- getArgs
  let mttrHost = Txt.pack targetHost
  let mttrPort = read targetPort
  let chanId = Txt.pack taegetChannelId
  res <- Wreq.get feedUri
  let theMsg = renderFeed <$> Import.parseFeedSource (res ^. Wreq.responseBody)
  mapM_ (postToMattermost MttrInfo {..} chanId) theMsg

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
