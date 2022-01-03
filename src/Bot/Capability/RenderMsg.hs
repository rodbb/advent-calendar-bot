module Bot.Capability.RenderMsg where

import Bot.Data.AdventCalendar (AdventCalendar)
import Data.Text (Text)

class (Monad m) => RenderMsg m where
  render :: FilePath -> AdventCalendar -> m Text
