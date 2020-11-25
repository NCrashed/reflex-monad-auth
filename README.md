# reflex-monad-auth

The package provides utilities to build in authorization in reflex application in
agnostic way to the concrete authorization scheme.

Features:
- Split application into two contexts: authorized and not authorized. Provides
  helpers to dynamically switch between both.
- Access to authorization specific state in authorized part of FRP network.

# How to use

See (example)[./example/Main.hs] and run it with `cabal new-run -f examples`.

First, define you own authorization type:

``` haskell
data JWTAuth = JWTAuth {
    authToken :: !Text
  , authRole  :: !Role
  } deriving (Eq)
```

Use it in `info` type hole in `AuthT` monad transformer:
``` haskell
runAuthJWT :: (Reflex t, TriggerEvent t m, MonadIO m) => AuthT JWTAuth t m a -> m a
runAuthJWT = runAuth

main :: IO ()
main = mainWidgetWithCss css $ runAuthJWT frontend
```

Now you can use the main tool of the package `liftAuth`:

``` haskell
frontend :: (HasAuth t m, PerformEvent t m, TriggerEvent t m, MonadHold t m, DomBuilder t m, PostBuild t m, MonadIO m, MonadIO (Performable m), AuthInfo t m ~ JWTAuth) => m ()
frontend = void $ liftAuth notlogged logged
  where
    notlogged = do
      pressE <- buttonClass "outline" "Login"
      widgetHold_ (pure ()) $ text "Logging in..." <$ pressE
      signE <- delay 1 pressE
      loginE <- requestLoginFromServer signE -- Here you widget that ask server for token
      void $ signin loginE
    logged = do
      pressE <- buttonClass "outline" "Logout"
      text "We are authorized!"
      void $ signout pressE
```

# Hacking

To enter shell with all dependencies for GHC, use `./ghc.sh` script. Or use `./ghcjs.sh` for GHCJS.
