module Bot.Capability.Summarize where

import Data.Text (Text)

class (Monad m) => Summarize m where
  summarize :: Text -> m Text
