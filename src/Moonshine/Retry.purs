module Moonshine.Retry where

import Prelude

import Control.Monad.Aff (delay)
import Control.Monad.Eff.Now as Now
import Data.DateTime.Instant as Inst
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Run as R
import Run.Except as RE

data Catch e a = Catch (Maybe e → a)
derive instance functorCatch ∷ Functor (Catch e)
_catch = SProxy ∷ SProxy "catch"
type CATCH e = R.FProxy (Catch e)
type WithCatch r e  = R.Run (catch ∷ CATCH e|r)

catch ∷ ∀ e a r. (e → WithCatch r e a) → WithCatch r e a → WithCatch r e a
catch handler attempt = do
  mbErr ← R.lift _catch $ Catch id
  case mbErr of
    Just e → handler e
    Nothing → attempt

reverseCatch ∷ ∀ a e r. e → WithCatch (except ∷ RE.EXCEPT e|r) e a → WithCatch (except ∷ RE.EXCEPT e|r) e Unit
reverseCatch defErr attempt = do
  mbErr ← R.lift _catch $ Catch id
  case mbErr of
    Just e → pure unit
    Nothing → RE.throw defErr

runCatch
  ∷ ∀ r e a
  . R.Run (except ∷ RE.EXCEPT e, catch ∷ CATCH e|r) a
  → R.Run r (Either e a)
runCatch = loop (pure <<< Left)
  where
  split =
    R.on _catch Right
    $ R.on RE._except
        (Left <<< Right)
        (Left <<< Left)
  loop hndl r = case R.peel r of
    Right a →
      pure $ Right a
    Left f → case split f of
      Right (Catch cont) →
        loop (\e → loop hndl $ cont $ Just e) $ cont Nothing
      Left (Right (RE.Except err)) →
        hndl err
      Left (Left others) →
        loop hndl =<< R.send others

type RetryState e =
  { step ∷ Milliseconds
  , total ∷ Milliseconds
  , defaultError ∷ e
  }

data RetryStateF e a
  = GetRetryState (RetryState e → a)
  | SetRetryState (RetryState e) a
derive instance functorRetryStateF ∷ Functor (RetryStateF e)
_retryState = SProxy ∷ SProxy "retryState"
type RETRY_STATE e = R.FProxy (RetryStateF e)
type WithRetryState e r = R.Run (retryState ∷ RETRY_STATE e|r)

liftRetryState ∷ ∀ r e. RetryStateF e ~> WithRetryState e r
liftRetryState = R.lift _retryState

getRetryState ∷ ∀ r e. WithRetryState e r (RetryState e)
getRetryState = liftRetryState $ GetRetryState id

setRetryState ∷ ∀ r e. RetryState e → WithRetryState e r Unit
setRetryState rs = liftRetryState $ SetRetryState rs unit

type RunCatch e r eff = R.Run
  ( eff ∷ R.EFF (now ∷ Now.NOW|eff)
  , aff ∷ R.AFF (now ∷ Now.NOW|eff)
  , except ∷ RE.EXCEPT e
  , catch ∷ CATCH e
  , retryState ∷ RETRY_STATE e
  |r )

runRetryState ∷ ∀ r a e. RetryState e → WithRetryState e r a → R.Run r a
runRetryState = loop
  where
  loop s r = case R.peel r of
    Right a → pure a
    Left f → case R.on _retryState Left Right f of
      Right others → R.send others >>= loop s
      Left (GetRetryState cont) → loop s (cont s)
      Left (SetRetryState newS next) → loop newS next

retry ∷ ∀ r e eff. RunCatch e r eff ~> RunCatch e r eff
retry try = getMSNum >>= attempt
  where
  getMSNum = R.liftEff $ map (un Milliseconds <<< Inst.unInstant) Now.now
  attempt t = flip catch try \err → do
    { total, step } ← getRetryState
    current ← getMSNum
    when (current - t > un Milliseconds total) $ RE.throw err
    R.liftAff $ delay step
    attempt t

reverse ∷ ∀ r e eff a. RunCatch e r eff a → RunCatch e r eff Unit
reverse attempt = do
  { defaultError } ← getRetryState
  reverseCatch defaultError attempt

await
  ∷ ∀ r e eff a b
  . RunCatch e r eff a
  → (a → RunCatch e r eff b)
  → RunCatch e r eff b
await a b = retry a >>= b

awaitNot
  ∷ ∀ r e eff a b
  . RunCatch e r eff a
  → RunCatch e r eff b
  → RunCatch e r eff b
awaitNot a b = reverse a *> b
