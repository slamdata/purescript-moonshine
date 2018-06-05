module Moonshine
  ( module Log
  , module Expect
  , module Retry
  , module Selector
  , module Moonshine
  ) where

import Prelude

import Data.Either (Either)
import Data.Traversable as T
import Effect.Now as Now
import Lunapark as L
import Lunapark.Types as LT
import Moonshine.Expect as Expect
import Moonshine.Log as Log
import Moonshine.Retry as Retry
import Moonshine.Selector as Selector
import Run as R
import Run.Except as RE

type RunMoonshine r eff = R.Run
  ( aff ∷ R.AFF
  , effect ∷ R.EFFECT
  , lunapark ∷ L.LUNAPARK
  , lunaparkActions ∷ L.LUNAPARK_ACTIONS
  , expect ∷ Expect.EXPECT LT.Element
  , selector ∷ Selector.SELECTOR LT.Element
  , except ∷ RE.EXCEPT L.Error
  , catch ∷ Retry.CATCH L.Error
  , retryState ∷ Retry.RETRY_STATE L.Error
  | r )

runMoonshine
  ∷ ∀ r eff a
  . String
  → LT.CapabilitiesRequest
  → Retry.RetryState L.Error
  → RunMoonshine r eff a
  → R.Run
      ( aff ∷ R.AFF
      , effect ∷ R.EFFECT |r)
      (Either L.Error Unit)
runMoonshine uri caps state action = Retry.runCatch do
  eInterpret ← L.init uri caps
  void $ T.for eInterpret \interpret →
    interpret
      $ Selector.runSelector
      $ Expect.runExpect
      $ Retry.runRetryState state
      $ Retry.catch (\_ → L.quit) (void action)
