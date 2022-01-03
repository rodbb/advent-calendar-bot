module Bot.Util where

import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Except (except)

hoistMaybe :: (Monad m) => e -> Maybe a -> ExceptT e m a
hoistMaybe e = except . note e

note :: e -> Maybe a -> Either e a
note _ (Just a) = Right a
note e Nothing = Left e
