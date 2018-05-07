module Moonshine.Log where

import Prelude

import Control.Monad.Eff.Console (CONSOLE, log)
import Text.Chalky as TC
import Run as R

section ∷ ∀ r e. String → R.Run (eff ∷ R.EFF (console ∷ CONSOLE|e)|r) ~> R.Run (eff ∷ R.EFF (console ∷ CONSOLE|e)|r)
section ann action = do
  R.liftEff do
    log ""
    log $ TC.magenta bar
    log $ TC.magenta ann
    log $ TC.magenta bar
    log ""
  result ← action
  R.liftEff do
    log ""
    log $ TC.magenta bar
    log $ TC.green $ "OK: " <> ann <> " passed"
    log $ TC.magenta bar
    log ""
  pure result
  where
  bar = "========================================"
