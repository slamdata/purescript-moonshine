module Moonshine.Log where

import Prelude

import Effect.Console (log)
import Text.Chalky as TC
import Run as R

section ∷ ∀ r. String → R.Run (effect ∷ R.EFFECT|r) ~> R.Run (effect ∷ R.EFFECT|r)
section ann action = do
  R.liftEffect do
    log ""
    log $ TC.magenta bar
    log $ TC.magenta ann
    log $ TC.magenta bar
    log ""
  result ← action
  R.liftEffect do
    log ""
    log $ TC.magenta bar
    log $ TC.green $ "OK: " <> ann <> " passed"
    log $ TC.magenta bar
    log ""
  pure result
  where
  bar = "========================================"
