{-# LANGUAGE OverloadedStrings #-}

module Bot.Data.Gregorian where

import Text.Mustache ((~>))
import qualified Text.Mustache as Mstch

newtype Gregorian = Gregorian (Integer, Int, Int)
  deriving (Show)

instance Mstch.ToMustache Gregorian where
  toMustache (Gregorian (y, m, d)) =
    Mstch.object
      [ "year" ~> y,
        "month" ~> m,
        "day" ~> d
      ]
