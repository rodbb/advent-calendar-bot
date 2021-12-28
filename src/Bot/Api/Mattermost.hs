{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Mattermost where

import Control.Lens ((^?))
import Data.Aeson (ToJSON (toJSON), defaultOptions, genericToEncoding)
import Data.Aeson.Types (toEncoding)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Network.Wreq as Wreq

type WebhookUrl = String

postMsg :: WebhookUrl -> Text -> IO (Maybe ByteString)
postMsg webhookUrl theMsg = do
  res <- Wreq.post webhookUrl $ toJSON MttrmstMsg {text = theMsg}
  return (res ^? Wreq.responseBody)

newtype MttrmstMsg = MttrmstMsg {text :: Text}
  deriving (Generic)

instance ToJSON MttrmstMsg where
  toEncoding = genericToEncoding defaultOptions
