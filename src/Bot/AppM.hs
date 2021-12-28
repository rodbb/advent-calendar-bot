{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Bot.AppM where

import Bot.Api.Asahi (callSummarizeApi)
import qualified Bot.Api.Mattermost as Mttr (postMsg)
import qualified Bot.Api.Qiita as Qiita (getCalendarFeed)
import Bot.Capability.Cache (ManageCache (readCache, writeCache))
import Bot.Capability.FetchFeed (FetchFeed (fetchFeed))
import Bot.Capability.PostMsg (PostMsg (postMsg))
import Bot.Capability.RenderMsg (RenderMsg (render))
import Bot.Capability.Summarize (Summarize (summarize))
import Bot.Data.Args (Args (..))
import Control.Exception (catch, throwIO)
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Fail (MonadFail)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader, ReaderT (runReaderT), asks, guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT), exceptToMaybeT)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt (writeFile)
import qualified Paths_advent_calendar_bot as Paths
import System.IO.Error (isDoesNotExistError)
import qualified Text.Mustache as Mstch

newtype AppM out = AppM {unAppM :: ReaderT Args (MaybeT IO) out}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Args,
      MonadFail,
      MonadIO
    )

runAppM :: AppM out -> Args -> IO (Maybe out)
runAppM AppM {unAppM} = runMaybeT . runReaderT unAppM

hoistMaybe :: Maybe a -> AppM a
hoistMaybe = AppM . lift . MaybeT . return

instance ManageCache AppM where
  readCache = do
    path <- asks cachePath
    liftIO $ (read <$> readFile path) `catch` handleError
    where
      handleError e = do
        if isDoesNotExistError e
          then return mempty
          else throwIO e
  writeCache cache = do
    path <- asks cachePath
    liftIO $ Txt.writeFile path $ Txt.pack (show cache)

instance FetchFeed AppM where
  fetchFeed = liftIO . Qiita.getCalendarFeed

instance Summarize AppM where
  summarize t = AppM $ do
    mApiUrl <- asks summaryApiUrl
    mApiKey <- asks summaryApiKey
    lift $ do
      apiUrl <- hoistM mApiUrl
      apiKey <- hoistM mApiKey
      liftIO $ callSummarizeApi apiUrl apiKey t
    where
      hoistM = MaybeT . return

instance RenderMsg AppM where
  render templatePath viewModel = AppM $ do
    dataDir <- liftIO Paths.getDataDir
    template <- lift . exceptToMaybeT $ ExceptT $ Mstch.automaticCompile [".", dataDir] templatePath
    return (Mstch.substitute template viewModel)

instance PostMsg AppM where
  postMsg msg = AppM $ do
    hookUrl <- asks mttrWebhookUrl
    lift $ MaybeT (Mttr.postMsg hookUrl msg)
