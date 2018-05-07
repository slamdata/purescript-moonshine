module Test.Main where

import Prelude

import CSS as CSS
import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Data.Time.Duration (Milliseconds(..))
import Lunapark as LP
import Lunapark.Error as LE
import Lunapark.Types as LT
import Moonshine as M
import Run as R
import Text.Chalky as CH

main ∷ Eff _ Unit
main = launchAff_ do
  eitherResult ← R.runBaseAff' $ runMoonshine do
    LP.go "http://localhost:8080"
    M.await  (M.withText "Skip video") LP.clickElement
    M.await (LP.findElement $ LT.ByCss $ CSS.fromString ".sd-notification-dismiss") LP.clickElement
    R.liftEff $ log $ CH.green "** Ok, launched SlamData **"
  pure unit
  where
  runMoonshine = M.runMoonshine
    "http://localhost:4444/wd/hub"
    { alwaysMatch: [], firstMatch: [[LT.BrowserName LT.Chrome]] }
    { defaultError: { error: LE.InvalidSelector, message: "The element shouldn't be presented but it is", stacktrace: "" }
    , step: Milliseconds 100.0
    , total: Milliseconds 10000.0
    }
