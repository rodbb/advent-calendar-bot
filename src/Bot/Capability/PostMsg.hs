module Bot.Capability.PostMsg where

import Data.Text (Text)

class (Monad m) => PostMsg m where
  postMsg :: Text -> m Text
