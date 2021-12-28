module Bot.Capability.PostMsg where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)

class (Monad m) => PostMsg m where
  postMsg :: Text -> m ByteString
