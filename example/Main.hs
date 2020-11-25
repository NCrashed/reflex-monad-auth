{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import Control.Monad.IO.Class
import Data.Functor
import Data.Map.Strict (Map)
import Data.Text (Text)
import Reflex.Auth
import Reflex.Dom
import Text.RawString.QQ

runAuthBool :: (Reflex t, TriggerEvent t m, MonadIO m) => AuthT Bool t m a -> m a
runAuthBool = runAuth

deriving instance DomBuilder t m => DomBuilder t (AuthT info t m)

main :: IO ()
main = mainWidgetWithCss css $ runAuthBool frontend
  where
    css = [r|
      .outline {
        border-width: 1px;
        border-radius: 10px;
        min-width: 100px;
        min-height: 50px;
        margin-right: 30px;
        color: black;
        background-color: white;
        border-color: black;
        font-size: 14pt;
      }
    |]

frontend :: (HasAuth t m, PerformEvent t m, TriggerEvent t m, MonadHold t m, DomBuilder t m, PostBuild t m, MonadIO m, MonadIO (Performable m), AuthInfo t m ~ Bool) => m ()
frontend = void $ liftAuth notlogged logged
  where
    notlogged = do
      pressE <- buttonClass "outline" "Login"
      widgetHold_ (pure ()) $ text "Logging in..." <$ pressE
      signE <- delay 1 pressE
      void $ signin (True <$ signE)
    logged = do
      pressE <- buttonClass "outline" "Logout"
      text "We are authorized!"
      void $ signout pressE

buttonClass :: (DomBuilder t m, PostBuild t m)
  => Dynamic t Text -> Dynamic t Text -> m (Event t ())
buttonClass classValD lbl = mkButton "button" [("onclick", "return false;")] classValD $ dynText lbl

mkButton :: (DomBuilder t m, PostBuild t m) => Text -> Map Text Text -> Dynamic t Text -> m a -> m (Event t a)
mkButton eltp attrs classValD ma = do
  let classesD = do
        classVal <- classValD
        pure $ attrs <> [("class", classVal)]
  (e, a) <- elDynAttr' eltp classesD ma
  return $ a <$ domEvent Click e
