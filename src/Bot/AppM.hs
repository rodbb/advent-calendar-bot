{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Bot.AppM (AppM, runAppM, (<?)) where

import Bot.Api.Asahi (callSummarizeApi)
import qualified Bot.Api.Mattermost as Mttr (postMsg)
import qualified Bot.Api.Qiita as Qiita (getCalendarFeed)
import Bot.Capability.Cache (ManageCache (readCache, writeCache))
import Bot.Capability.FetchFeed (FetchFeed (fetchFeed))
import Bot.Capability.PostMsg (PostMsg (postMsg))
import Bot.Capability.RenderMsg (RenderMsg (render))
import Bot.Capability.Summarize (Summarize (summarize))
import Bot.Data.Args (Args (..))
import qualified Bot.Util as Util
import Control.Applicative (Alternative)
import Control.Exception (catch, throwIO)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, withExceptT)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader, ReaderT (runReaderT), asks, guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (except)
import qualified Data.ByteString.Lazy as BL (toStrict)
import Data.String.Conversions (convertString)
import Data.Text (Text)
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt (writeFile)
import System.IO.Error (isDoesNotExistError)
import qualified Text.Mustache as Mstch
import Text.Parsec.Error (ParseError, showErrorMessages)
import TextShow (TextShow (showt))

newtype AppM out = AppM {unAppM :: ReaderT Args (ExceptT Text IO) out}
  deriving
    ( Functor,
      Applicative,
      Alternative,
      Monad,
      MonadReader Args,
      MonadFail,
      MonadIO
    )

runAppM :: AppM out -> Args -> IO (Either Text out)
runAppM AppM {unAppM} = runExceptT . runReaderT unAppM

hoistIOEither :: TextShow e => IO (Either e a) -> AppM a
hoistIOEither = AppM . lift . withExceptT showt . ExceptT

hoistEither :: TextShow e => Either e a -> AppM a
hoistEither = hoistIOEither . return

(<?) :: TextShow e => Maybe a -> e -> AppM a
m <? err = hoistEither (Util.note err m)

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
  fetchFeed = AppM . lift . ExceptT . Qiita.getCalendarFeed

instance Summarize AppM where
  summarize t = do
    mApiUrl <- asks summaryApiUrl
    mApiKey <- asks summaryApiKey
    apiUrl <- mApiUrl <? ("summaryApiUrl is required" :: Text)
    apiKey <- mApiKey <? ("summaryApiKey is required" :: Text)
    AppM . lift $ callSummarizeApi apiUrl apiKey t

instance RenderMsg AppM where
  render templatePath viewModel = AppM $ do
    let compile = Mstch.localAutomaticCompile templatePath
    template <- lift $ withExceptT (Txt.pack . show) $ ExceptT compile
    return (Mstch.substitute template viewModel)

instance PostMsg AppM where
  postMsg msg = AppM $ do
    hookUrl <- asks mttrWebhookUrl
    ret <- lift $ ExceptT (Mttr.postMsg hookUrl msg)
    return (convertString ret)
