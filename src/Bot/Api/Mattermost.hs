{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Mattermost where

import Bot.Api.Util (ReqInfo, reqPost, useStr, customHttpConfig)
import qualified Bot.Util as Util
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Aeson (ToJSON, defaultOptions, genericToEncoding)
import Data.Aeson.Types (toEncoding)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Network.HTTP.Req as Req

type WebhookUrl = String

postMsg :: Bool -> WebhookUrl -> Text -> IO (Either Text ByteString)
postMsg insecure webhookUrl theMsg = runExceptT $ do
  eUrlInfo <- Util.hoistMaybe "Invalid API URL" (useStr webhookUrl)
  either post post eUrlInfo
  where
    post :: ReqInfo scheme -> ExceptT Text IO ByteString
    post info = do
      let body = Req.ReqBodyJson MttrmstMsg {text = theMsg}
      let req = reqPost info body Req.lbsResponse
      httpConf <- customHttpConfig insecure
      Req.responseBody <$> Req.runReq httpConf req

newtype MttrmstMsg = MttrmstMsg {text :: Text}
  deriving (Generic)

instance ToJSON MttrmstMsg where
  toEncoding = genericToEncoding defaultOptions
