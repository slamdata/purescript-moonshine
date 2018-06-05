module Test.Main where

import Prelude

import CSS as CSS
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console (log)
import Lunapark as LP
import Lunapark.Error as LE
import Lunapark.Types as LT
import Moonshine as M
import Run as R
import Text.Chalky as CH

main ∷ Effect Unit
main = launchAff_ do
  eitherResult ← R.runBaseAff' $ runMoonshine do
    LP.go "http://localhost:8080"
    M.await  (M.withText "Skip video") LP.clickElement
    M.await (LP.findElement $ LT.ByCss $ CSS.fromString ".sd-notification-dismiss") LP.clickElement
    R.liftEffect $ log $ CH.green "** Ok, launched SlamData **"
  pure unit
  where
  runMoonshine = M.runMoonshine
    "http://localhost:4444/wd/hub"
    { alwaysMatch: [], firstMatch: [[LT.BrowserName LT.Chrome]] }
    { defaultError: { error: LE.InvalidSelector, message: "The element shouldn't be presented but it is", stacktrace: "" }
    , step: Milliseconds 100.0
    , total: Milliseconds 10000.0
    }
