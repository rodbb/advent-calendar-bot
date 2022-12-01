{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Api.Asahi where

import Bot.Api.Util (ReqInfo, reqPost, useStr, customHttpConfig)
import qualified Bot.Util as Util
import Control.Monad.Except (ExceptT)
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
import qualified Network.HTTP.Req as Req
import Prelude hiding (length)

callSummarizeApi :: Bool -> String -> String -> Text -> ExceptT Text IO Text
callSummarizeApi insecure url apiKey ec = do
  eUrlInfo <- Util.hoistMaybe "Invalid API URL" (useStr url)
  either post post eUrlInfo
  where
    post :: ReqInfo scheme -> ExceptT Text IO Text
    post (url, opt) = do
      let body = Req.ReqBodyJson ApiRequestBody {text = ec, length = 1000}
      let opts = opt <> Req.header "x-api-key" (B.pack apiKey)
      let req = reqPost (url, opts) body Req.jsonResponse
      httpConf <- customHttpConfig insecure
      fold . result . Req.responseBody <$> Req.runReq httpConf req

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
