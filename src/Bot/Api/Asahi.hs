{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Api.Asahi where

import Bot.Api.Util (ReqInfo, reqPost, useStr)
import qualified Bot.Util as Util
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
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

callSummarizeApi :: String -> String -> Text -> MaybeT IO Text
callSummarizeApi url apiKey ec = do
  eUrlInfo <- Util.hoistMaybe (useStr url)
  either post post eUrlInfo
  where
    post :: ReqInfo scheme -> MaybeT IO Text
    post (url, opt) = do
      let body = Req.ReqBodyJson ApiRequestBody {text = ec, length = 1000}
      let opts = opt <> Req.header "x-api-key" (B.pack apiKey)
      let req = reqPost (url, opts) body Req.jsonResponse
      fold . result . Req.responseBody <$> Req.runReq Req.defaultHttpConfig req

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
