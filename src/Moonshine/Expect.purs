module Moonshine.Expect where

import Prelude

import CSS (fromString)
import Data.Argonaut.Decode.Class as J
import Data.Array as A
import Data.Either (Either(..))
import Data.Newtype (un)
import Data.String as Str
import Data.Symbol (SProxy(..))
import Data.Traversable as T
import Lunapark as LP
import Lunapark.Types as LT
import Lunapark.Utils (throwLeft)
import Run as R
import Run.Except as RE

newtype TableDescription = TableDescription Void

data ExpectF el a
  = Exists el a
  | IsChecked el a
  | IsEnabled el a
  | IsPresented el a
  | HasSelection String el a
  | Table TableDescription el a

derive instance functorExpectF ∷ Functor (ExpectF el)

_expect = SProxy ∷ SProxy "expect"
type EXPECT el = R.FProxy (ExpectF el)
type WithExpect el r a = R.Run (expect ∷ EXPECT el|r) a

liftExpect ∷ ∀ el r. ExpectF el ~> WithExpect el r
liftExpect = R.lift _expect

exists ∷ ∀ el r. el → WithExpect el r Unit
exists el = liftExpect $ Exists el unit

isChecked ∷ ∀ el r. el → WithExpect el r Unit
isChecked el = liftExpect $ IsChecked el unit

isPresented ∷ ∀ el r. el → WithExpect el r Unit
isPresented el = liftExpect $ IsPresented el unit

isEnabled ∷ ∀ el r. el → WithExpect el r Unit
isEnabled el = liftExpect $ IsEnabled el unit

hasSelection ∷ ∀ el r. String → el → WithExpect el r Unit
hasSelection txt el = liftExpect $ HasSelection txt el unit

table ∷ ∀ el r. TableDescription → el → WithExpect el r Unit
table descr el = liftExpect $ Table descr el unit

runExpect
  ∷ ∀ r
  . WithExpect LT.Element (lunapark ∷ LP.LUNAPARK, except ∷ RE.EXCEPT LP.Error|r)
  ~> R.Run (lunapark ∷ LP.LUNAPARK, except ∷ RE.EXCEPT LP.Error |r)
runExpect = R.interpretRec (R.on _expect handleExpect R.send)
  where
  handleExpect ∷ ExpectF LT.Element ~> R.Run (lunapark ∷ LP.LUNAPARK, except ∷ RE.EXCEPT LP.Error |r)
  handleExpect = case _ of
    Exists el next → do
      j ← LP.executeScript { script: "var el = arguments[0]; return !!el", args: [LT.encodeElement el ] }
      res ← throwLeft $ J.decodeJson j
      unless res $ RE.throw
        { error: LP.StaleElementReference
        , message: "The element " <> un LT.Element el <> " doesn't exist"
        , stacktrace: ""
        }
      pure next
    IsPresented el next → do
      res ← LP.isDisplayed el
      unless res $ RE.throw
        { error: LP.UnknownError
        , message: "The element " <> un LT.Element el <> " is not presented"
        , stacktrace: ""
        }
      pure next
    IsChecked el next → do
      res ← LP.isSelected el
      unless res $ RE.throw
        { error: LP.UnknownError
        , message: "The element " <> un LT.Element el <> " is not selected"
        , stacktrace: ""
        }
      pure next
    IsEnabled el next → do
      res ← LP.isEnabled el
      unless res $ RE.throw
        { error: LP.UnknownError
        , message: "The element " <> un LT.Element el <> " is not enabled"
        , stacktrace: ""
        }
      pure next
    HasSelection txt el next → do
      opts ← LP.childElements el $ LT.ByCss $ fromString $ "option[text='" <> txt <> "']"
      inputs ← LP.childElements el $ LT.ByCss $ fromString "input"
      values ← T.for inputs $ flip LP.getProperty "value"
      let compatibles = flip T.foldMap values \j → case J.decodeJson j of
            Left _ → [ ]
            Right c → if Str.contains (Str.Pattern txt) c then [c] else [ ]
      unless (A.length compatibles + A.length opts > 0) $ RE.throw
        { error: LP.UnknownError
        , message: "The element " <> un LT.Element el <> " has now " <> txt <> " selection"
        , stacktrace: ""
        }
      pure next
    Table _ _ next → pure next
