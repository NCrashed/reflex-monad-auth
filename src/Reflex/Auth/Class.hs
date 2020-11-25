module Reflex.Auth.Class(
    HasAuth(..)
  , AuthedEnv
  , getAuthInfoMay
  , getLogged
  , signout
  , signin
  ) where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Maybe
import Reflex
import Reflex.ExternalRef

-- | Environment that holds changing authorization information
type AuthedEnv t m = Dynamic t (AuthInfo t m)

-- | Monad for actions that can be authorizaed or not authorized.
class (Eq (AuthInfo t m), Reflex t) => HasAuth t m | m -> t where
  -- | Monad specific information about authorization
  type AuthInfo t m :: *

  -- | Get reference from context that tracks current state of authorization
  getAuthInfoRef :: m (ExternalRef t (Maybe (AuthInfo t m)))

  -- | A typical way to perform one action when user logged and perform another
  -- FRP network part when the user is authorized.
  liftAuth :: m a -- ^ Which widget to render when not authorized
    -> ReaderT (AuthedEnv t m) m a -- ^ Which widget to render when authorized
    -> m (Dynamic t a)

-- | Get dynamic that is 'Nothing' when is not authorized and 'Just' when we have authorization.
getAuthInfoMay :: (HasAuth t m, MonadHold t m, MonadFix m, MonadIO m) => m (Dynamic t (Maybe (AuthInfo t m)))
getAuthInfoMay = holdUniqDyn =<< externalRefDynamic =<< getAuthInfoRef

-- | Return dynamic that indicates whether we in authorized or in authorized state.
getLogged :: (HasAuth t m, MonadHold t m, MonadFix m, MonadIO m) => m (Dynamic t Bool)
getLogged = fmap isJust <$> getAuthInfoMay

-- | When input event is fired, the auth info inside is nullified and all `liftAuth` widgets
-- are switched back to not authorized states.
signout :: (HasAuth t m, PerformEvent t m, MonadIO m, MonadIO (Performable m)) => Event t () -> m (Event t ())
signout e = do
  ref <- getAuthInfoRef
  performEvent $ ffor e $ const $ writeExternalRef ref Nothing

-- | When input event is fired all `liftAuth` widgets switches to authorized states.
signin :: (HasAuth t m, PerformEvent t m, MonadIO m, MonadIO (Performable m)) => Event t (AuthInfo t m) -> m (Event t (AuthInfo t m))
signin e = do
  ref <- getAuthInfoRef
  performEvent $ ffor e $ \ai -> writeExternalRef ref (Just ai) >> pure ai

instance {-# OVERLAPPABLE #-} (HasAuth t m, Monad m) => HasAuth t (ReaderT e m) where
  type AuthInfo t (ReaderT e m) = AuthInfo t m

  getAuthInfoRef = lift getAuthInfoRef
  {-# INLINE getAuthInfoRef #-}

  liftAuth unauthed authed = do
    e <- ask
    let authed' = do
          ae <- ask
          lift $ runReaderT (runReaderT authed ae) e
    lift $ liftAuth (runReaderT unauthed e) authed'
  {-# INLINE liftAuth #-}
