module Moonshine
  ( module Moonshine.Expect
  , module Moonshine.Log
  , module Moonshine.Retry
  , module Moonshine.Selector
  , module Moonshine
  ) where

import Prelude

import Data.Either (Either)
import Data.Traversable as T
import Lunapark as L
import Lunapark.Types as LT
import Moonshine.Expect (EXPECT, ExpectF(..), TableDescription(..), WithExpect, _expect, exists, hasSelection, isChecked, isEnabled, isPresented, liftExpect, runExpect, table)
import Moonshine.Log (section)
import Moonshine.Retry (CATCH, Catch(..), RETRY_STATE, RetryState, RetryStateF(..), RunCatch, WithCatch, WithRetryState, _catch, _retryState, await, awaitNot, catch, getRetryState, liftRetryState, retry, reverse, reverseCatch, runCatch, runRetryState, setRetryState)
import Moonshine.Selector (Attributes, Properties, SELECTOR, SelectorF(..), WithSelector, _selector, after, after_, before, beforeScript, before_, isAfter, isBefore, liftSelector, runSelector, withAttributes, withLabel, withProperties, withText, withTitle)
import Run as R
import Run.Except as RE

type RunMoonshine r = R.Run
  ( aff ∷ R.AFF
  , effect ∷ R.EFFECT
  , lunapark ∷ L.LUNAPARK
  , lunaparkActions ∷ L.LUNAPARK_ACTIONS
  , expect ∷ EXPECT LT.Element
  , selector ∷ SELECTOR LT.Element
  , except ∷ RE.EXCEPT L.Error
  , catch ∷ CATCH L.Error
  , retryState ∷ RETRY_STATE L.Error
  | r )

runMoonshine
  ∷ ∀ r a
  . String
  → LT.CapabilitiesRequest
  → RetryState L.Error
  → RunMoonshine r a
  → R.Run
      ( aff ∷ R.AFF
      , effect ∷ R.EFFECT |r)
      (Either L.Error Unit)
runMoonshine uri caps state action = runCatch do
  eInterpret ← L.init uri caps
  void $ T.for eInterpret \interpret →
    interpret
      $ runSelector
      $ runExpect
      $ runRetryState state
      $ catch (\_ → L.quit) (void action)
