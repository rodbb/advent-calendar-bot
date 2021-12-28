{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Api.Asahi where

import Control.Lens ((&), (.~), (^?), (^.))
import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding, toJSON),
    defaultOptions,
    genericToEncoding,
  )
import qualified Data.ByteString.Char8 as B
import Data.Foldable (fold)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Network.Wreq as Wreq
import Prelude hiding (length)

callSummarizeApi :: String -> String -> Text -> IO Text
callSummarizeApi url apiKey ec = do
  let reqBody = ApiRequestBody {text = ec, length = 1000}
  let opts = Wreq.defaults & Wreq.header "x-api-key" .~ [B.pack apiKey]
  res <- Wreq.asJSON =<< Wreq.postWith opts url (toJSON reqBody)
  return $ (fold . result) (res ^. Wreq.responseBody)

data ApiRequestBody = ApiRequestBody
  { text :: Text,
    length :: Int
  }
  deriving (Generic)

instance ToJSON ApiRequestBody where
  toEncoding = genericToEncoding defaultOptions

newtype ApiResponseBody = ApiResponseBody {result :: [Text]}
  deriving (Generic)

instance FromJSON ApiResponseBody
