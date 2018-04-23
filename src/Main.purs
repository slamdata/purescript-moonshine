module Main where

import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.Symbol (SProxy(..))
import Text.Chalky as TC
import Lunapark.Types as LT
import Run as R
import Unsafe.Coerce (unsafeCoerce)

import Debug.Trace as DT

data LogF r a
  = Section String (WithLog r a)
  | Debug String a
  | Success String a
  | Error String a
  | Warn String a

derive instance functorLogF ∷ Functor (LogF r)

_log = SProxy ∷ SProxy "log"
type LOG r = R.FProxy (LogF r)
type WithLog r a = R.Run (log ∷ LOG r|r) a

liftLog ∷ ∀ r. LogF r ~> WithLog r
liftLog = R.lift _log

section ∷ ∀ r. String → WithLog r ~> WithLog r
section str action = liftLog $ Section str action

logDebug ∷ ∀ r. String → WithLog r Unit
logDebug str = liftLog $ Debug str unit

logSuccess ∷ ∀ r. String → WithLog r Unit
logSuccess str = liftLog $ Success str unit

logError ∷ ∀ r. String → WithLog r Unit
logError str = liftLog $ Error str unit

logWarn ∷ ∀ r. String → WithLog r Unit
logWarn str = liftLog $ Warn str unit

interpret
  ∷ ∀ e r
  . WithLog (eff ∷ R.EFF (console ∷ CONSOLE|e)|r)
  ~> R.Run (eff ∷ R.EFF (console ∷ CONSOLE|e)|r)
interpret = loop
  where
  handle = R.on _log Left Right
  bar = "========================================"
  loop r = case R.peel r of
    Left a → case handle a of
      Left logF → case logF of
        Section ann others → do
          R.liftEff do
            log ""
            log $ TC.magenta bar
            log $ TC.magenta ann
            log $ TC.magenta bar
            log ""
          result ← loop =<< interpret others
          R.liftEff do
            log ""
            log $ TC.magenta bar
            log $ TC.green $ "OK: " <> ann <> " passed"
            log $ TC.magenta bar
            log ""
          pure result
        Debug txt next → do
          R.liftEff do
            log $ TC.gray bar
            log $ TC.gray txt
            log $ TC.gray bar
          loop next
        Success txt next → do
          R.liftEff do
            log $ TC.green bar
            log $ TC.green txt
            log $ TC.green bar
          loop next
        Error txt next → do
          R.liftEff do
            log $ TC.red bar
            log $ TC.red txt
            log $ TC.red bar
          loop next
        Warn txt next → do
          R.liftEff do
            log $ TC.yellow bar
            log $ TC.yellow txt
            log $ TC.yellow bar
          loop next
      Right others →
        R.send others >>= interpret
    Right a →
      pure a


data SelectorF r el a
  = WithText String (el → a)
  | WithTitle String (el → a)
  | WithLabel String (el → a)
  | Before el (WithSelector r el el) (el → a)
  | After el (WithSelector r el el) (el → a)

derive instance functorSelectorF ∷ Functor (SelectorF r el)

_selector = SProxy ∷ SProxy "selector"
type SELECTOR r el = R.FProxy (SelectorF r el)
type WithSelector r el = R.Run (selector ∷ SELECTOR r el|r)

liftSelector ∷ ∀ r el. SelectorF el ~> WithSelector r el
liftSelector = R.lift _selector

withText ∷ ∀ r el. String → WithSelector r el el
withText txt = liftSelector $ WithText txt id

withTitle ∷ ∀ r el. String → WithSelector r el el
withTitle txt = liftSelector $ WithTitle txt id

withLabel ∷ ∀ r el. String → WithSelector r el el
withLabel txt = liftSelector $ WithLabel txt id

before ∷ ∀ r el. el → WithSelector r el el → WithSelector r el el
before el ac = liftSelector $ Before el ac id

after ∷ ∀ r el. el → WithSelector r el el → WithSelector r el el
after el ac = liftSelector $ After el ac id

runSelector
  ∷ ∀ e r
  . WithSelector (eff ∷ R.EFF (console ∷ CONSOLE|e)|r) LT.Element
  ~> R.Run (eff ∷ R.EFF (console ∷ CONSOLE|e)|r)
runSelector _ = pure $ unsafeCoerce unit


main ∷ ∀ e. Eff (console ∷ CONSOLE|e) Unit
main = void $ launchAff do
  R.runBaseAff' $ interpret do
    section "section" do
      R.liftEff $ log "ZZZ"
  DT.traceAnyA "FOO"
