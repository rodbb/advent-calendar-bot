module Bot.Util where

import Control.Monad.Trans.Maybe (MaybeT (MaybeT))

hoistMaybe :: (Monad m) => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return
