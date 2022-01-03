{-# LANGUAGE DataKinds #-}

module Bot.Api.Util where

import Data.Aeson (FromJSON)
import Data.Proxy (Proxy)
import qualified Data.Text as Txt
import qualified Network.HTTP.Req as Req
import Text.URI (mkURI)
import qualified Text.URI as URI

type URL = String

type ReqInfo scheme = (Req.Url scheme, Req.Option scheme)

useStr :: URL -> Maybe (Either (ReqInfo 'Req.Http) (ReqInfo 'Req.Https))
useStr str = Req.useURI =<< mkURI (Txt.pack str)

reqGet ::
  (Req.HttpResponse res, Req.MonadHttp m) =>
  ReqInfo scheme ->
  Proxy res ->
  m res
reqGet (url, opt) res = Req.req Req.GET url Req.NoReqBody res opt

reqPost ::
  (Req.HttpBody body, Req.HttpResponse res, Req.MonadHttp m) =>
  ReqInfo scheme ->
  body ->
  Proxy res ->
  m res
reqPost (url, opt) body res = Req.req Req.POST url body res opt
