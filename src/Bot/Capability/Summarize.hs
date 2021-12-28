module Bot.Capability.Summarize where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (lift)
import Data.Text (Text)

class (Monad m) => Summarize m where
  summarize :: Text -> m Text
