module Reflex.Auth.Trans(
    AuthEnv
  , AuthT(..)
  , newAuthEnv
  , runAuthT
  , runAuth
  , module Reflex.Auth.Class
  ) where

import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State.Strict
import Language.Javascript.JSaddle.Types
import GHC.Generics
import Reflex
import Reflex.Auth.Class
import Reflex.ExternalRef
import Reflex.Network

-- | Environment for `AuthT`
type AuthEnv t info = ExternalRef t (Maybe info)

-- | Allocate new authorization environment that is not logged by default
newAuthEnv :: (Reflex t, TriggerEvent t m, MonadIO m) => m (AuthEnv t info)
newAuthEnv = newExternalRef Nothing

-- | Monad that implements 'HasAuth' with simple reader inside
newtype AuthT info t m a = AuthT { unAuthT :: ReaderT (AuthEnv t info) m a }
  deriving (Functor, Applicative, Monad, Generic, MonadFix)

deriving instance PostBuild t m => PostBuild t (AuthT info t m)
deriving instance NotReady t m => NotReady t (AuthT info t m)
deriving instance PerformEvent t m => PerformEvent t (AuthT info t m)
deriving instance TriggerEvent t m => TriggerEvent t (AuthT info t m)
deriving instance MonadHold t m => MonadHold t (AuthT info t m)
deriving instance MonadSample t m => MonadSample t (AuthT info t m)
deriving instance MonadIO m => MonadIO (AuthT info t m)
#ifndef ghcjs_HOST_OS
deriving instance MonadJSM m => MonadJSM (AuthT info t m)
#endif
deriving instance (Group q, Additive q, Query q, Eq q, MonadQuery t q m, Monad m) => MonadQuery t q (AuthT info t m)
deriving instance (Monoid w, DynamicWriter t w m) => DynamicWriter t w (AuthT info t m)
deriving instance (Monoid w, BehaviorWriter t w m) => BehaviorWriter t w (AuthT info t m)
deriving instance (Semigroup w, EventWriter t w m) => EventWriter t w (AuthT info t m)
deriving instance (Requester t m) => Requester t (AuthT info t m)

instance MonadTrans (AuthT info t) where
  lift = AuthT . lift
  {-# INLINABLE lift #-}

instance MonadReader e m => MonadReader e (AuthT info t m) where
  ask = lift ask
  {-# INLINABLE ask #-}
  local f (AuthT ma) = AuthT $ do
    r <- ask
    lift $ local f $ runReaderT ma r
  {-# INLINABLE local #-}

instance MonadState s m => MonadState s (AuthT info t m) where
  get = lift get
  {-# INLINABLE get #-}
  put = lift . put
  {-# INLINABLE put #-}

instance Adjustable t m => Adjustable t (AuthT info t m) where
  runWithReplace a0 a' = do
    r <- AuthT ask
    lift $ runWithReplace (runAuthT a0 r) $ fmap (`runAuthT` r) a'
  {-# INLINABLE runWithReplace #-}
  traverseIntMapWithKeyWithAdjust f dm0 dm' = do
    r <- AuthT ask
    lift $ traverseIntMapWithKeyWithAdjust (\k v -> runAuthT (f k v) r) dm0 dm'
  {-# INLINABLE traverseIntMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust f dm0 dm' = do
    r <- AuthT ask
    lift $ traverseDMapWithKeyWithAdjust (\k v -> runAuthT (f k v) r) dm0 dm'
  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = do
    r <- AuthT ask
    lift $ traverseDMapWithKeyWithAdjustWithMove (\k v -> runAuthT (f k v) r) dm0 dm'
  {-# INLINABLE traverseDMapWithKeyWithAdjustWithMove #-}

-- | Execute widget with authorization logic with given environment.
runAuthT :: AuthT info t m a -> AuthEnv t info -> m a
runAuthT (AuthT ma) e = runReaderT ma e
{-# INLINEABLE runAuthT #-}

-- | Simplified version of `runAuthT`
runAuth :: (Reflex t, TriggerEvent t m, MonadIO m) => AuthT info t m a -> m a
runAuth ma = do
  re <- newAuthEnv
  runAuthT ma re
{-# INLINABLE runAuth #-}

instance (Eq info, Reflex t, MonadIO m, MonadHold t m, MonadFix m, Adjustable t m) => HasAuth t (AuthT info t m) where
  type AuthInfo t (AuthT info t m) = info

  getAuthInfoRef = AuthT ask
  {-# INLINE getAuthInfoRef #-}

  liftAuth unauth authed = do
    ref <- AuthT ask
    ai0 <- readExternalRef ref
    aimd <- holdUniqDyn =<< externalRefDynamic ref
    aid <- fmap fromJust <$> improvingMaybe aimd
    let
      mauthed = runReaderT authed aid
      m = maybe unauth (const mauthed)
    networkHold (m ai0) $ m <$> updated aimd
    where
      fromJust Nothing = error "liftAuth: impossible, forced Nothing in authed dynamic"
      fromJust (Just a) = a
