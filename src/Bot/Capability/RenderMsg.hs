module Bot.Capability.RenderMsg where

import Bot.Data.AdventCalendar (AdventCalendar)
import Data.Text (Text)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (lift)

class (Monad m) => RenderMsg m where
  render :: FilePath -> AdventCalendar -> m Text
