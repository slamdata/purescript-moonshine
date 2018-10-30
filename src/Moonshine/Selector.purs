module Moonshine.Selector where

import Prelude

import CSS (fromString)
import Data.Argonaut.Core (Json, isNull) as J
import Data.Argonaut.Decode.Class (decodeJson) as J
import Data.Argonaut.Encode.Class (encodeJson) as J
import Data.Array as A
import Data.List as L
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.XPath.Builder ((==.), (|.), (</>))
import Data.XPath.Builder as X
import Data.XPath.Builder.Function as XF
import Lunapark as LP
import Lunapark.Error as LE
import Lunapark.Types as LT
import Lunapark.Utils (catch, throwLeft)
import Run as R
import Run.Except as RE

type Attributes = Map.Map String (Maybe String)
type Properties = Map.Map String J.Json

data SelectorF el a
  = WithLabel String (el → a)
  | WithTitle String (el → a)
  | WithText String (el → a)
  | Before el el (el → a)
  | After el el (el → a)
  | WithProperties el Properties (el → a)
  | WithAttributes el Attributes (el → a)

derive instance functorSelectorF ∷ Functor (SelectorF el)

_selector = SProxy ∷ SProxy "selector"
type SELECTOR el = R.FProxy (SelectorF el)
type WithSelector el r a = R.Run (selector ∷ SELECTOR el|r) a

liftSelector ∷ ∀ el r. SelectorF el ~> WithSelector el r
liftSelector = R.lift _selector

withLabel ∷ ∀ el r. String → WithSelector el r el
withLabel txt = liftSelector $ WithLabel txt identity

withTitle ∷ ∀ el r. String → WithSelector el r el
withTitle txt = liftSelector $ WithTitle txt identity

withText ∷ ∀ el r. String → WithSelector el r el
withText txt = liftSelector $ WithText txt identity

before ∷ ∀ el r. el → el → WithSelector el r el
before el a = liftSelector $ Before el a identity

after ∷ ∀ el r. el → el → WithSelector el r el
after el a = liftSelector $ After el a identity

before_ ∷ ∀ el r. WithSelector el r el → WithSelector el r el → WithSelector el r el
before_ a b = do
  a' ← a
  b' ← b
  before a' b'

after_ ∷ ∀ el r. WithSelector el r el → WithSelector el r el → WithSelector el r el
after_ a b = do
  a' ← a
  b' ← b
  after a' b'

withProperties ∷ ∀ el r. el → Properties → WithSelector el r el
withProperties el props = liftSelector $ WithProperties el props identity

withAttributes ∷ ∀ el r. el → Attributes → WithSelector el r el
withAttributes el attrs = liftSelector $ WithAttributes el attrs identity

runSelector
  ∷ ∀ r
  . WithSelector LT.Element (lunapark ∷ LP.LUNAPARK, except ∷ RE.EXCEPT LE.Error|r)
  ~> R.Run (lunapark ∷ LP.LUNAPARK, except ∷ RE.EXCEPT LE.Error|r)
runSelector = R.interpretRec (R.on _selector handleSelector R.send)
  where
  handleSelector
    ∷ SelectorF LT.Element
    ~> R.Run (lunapark ∷ LP.LUNAPARK, except ∷ RE.EXCEPT LE.Error|r)
  handleSelector = case _ of
    WithText txt cont →
      let
       byText =
         LP.findElement $ LT.ByXPath
           $ X.absDescendantOrSelf $ X.withPredicate X.any (X.text ==. txt)

       inputValFoldFn (Just el) _ = pure $ Just el
       inputValFoldFn Nothing el = do
         prop ← LP.getProperty el "value"
         if J.encodeJson txt == prop then pure $ Just el else pure Nothing

       byInputVal = do
         els ← LP.findElements $ LT.ByCss $ fromString "input"
         mbEl ← A.foldM inputValFoldFn Nothing els
         case mbEl of
           Nothing → RE.throw { error: LE.NoSuchElement, message: "There is no input with value = " <> txt, stacktrace: "" }
           Just el → pure el

       placeholderFoldFn (Just el) _ = pure $ Just el
       placeholderFoldFn Nothing el = do
         prop ← LP.getProperty el "value"
         if J.encodeJson "" == prop || J.isNull prop then pure $ Just el else pure Nothing

       byPlaceholder = do
         els ← LP.findElements $ LT.ByXPath
           $ X.absDescendantOrSelf
             (X.withPredicate  (X.named "input") (X.attr ( "placeholder") ==.  txt))
         mbEl ← A.foldM placeholderFoldFn Nothing els
         case mbEl of
           Nothing → RE.throw { error: LE.NoSuchElement, message: "There is no input with placeholder = " <> txt, stacktrace: "" }
           Just el → pure el

      in map cont $ catch byText \_ → catch byInputVal \_ → byPlaceholder

    WithTitle txt cont →
      let
        titled =
          X.absDescendantOrSelf
            (X.withPredicate X.any (X.attr ( "title") ==. txt))
        ariaLabelled =
          X.absDescendantOrSelf
            (X.withPredicate X.any (X.attr ( "aria-label") ==. txt))
      in map cont $ LP.findElement $ LT.ByXPath $ titled |. ariaLabelled
    WithLabel txt cont →
      let
        labelled =
          X.absDescendantOrSelf
            ((X.withPredicate
                (X.named "label")
                (XF.startsWith X.text txt) </> X.any) :: X.Path)
        labelledBy =
          X.absDescendantOrSelf
            (X.withPredicate
              X.any
              (X.withPredicate
                (XF.id (X.attr "aria-labelledby"))
                (X.text ==. txt)))
      in map cont $ LP.findElement $ LT.ByXPath $ labelled |. labelledBy
    Before el tested cont → do
      res ← isBefore el tested
      if res then pure $ cont tested else RE.throw
        { error: LE.NoSuchElement
        , message: "Required element isn't located before " <> un LT.Element el
        , stacktrace: ""
        }
    After el tested cont → do
      res ← isAfter el tested
      if res then pure $ cont tested else RE.throw
        { error: LE.NoSuchElement
        , message: "Required element isn't located before " <> un LT.Element el
        , stacktrace: ""
        }
    WithProperties el props cont → do
      let
        foldFn acc (Tuple key expected) =
          if not acc
          then pure acc
          else map (eq expected) $ LP.getProperty el key

      fits ← L.foldM foldFn true $ Map.toUnfoldable props
      if fits then pure $ cont el else RE.throw
        { error: LE.NoSuchElement
        , message: "The element " <> un LT.Element el <> " properties don't fit"
        , stacktrace: ""
        }
    WithAttributes el props cont → do
      let
        foldFn acc (Tuple key expected) = if not acc then pure acc else do
          prop ← catch (Just <$> LP.getAttribute el key) (\_ → pure Nothing)
          pure $ prop == expected
      fits ← L.foldM foldFn true $ Map.toUnfoldable props
      if fits then pure $ cont el else RE.throw
        { error: LE.NoSuchElement
        , message: "The element " <> un LT.Element el <> " attributes don't fit"
        , stacktrace: ""
        }

isBefore ∷ ∀ r. LT.Element → LT.Element → R.Run (lunapark ∷ LP.LUNAPARK, except ∷ RE.EXCEPT LE.Error|r) Boolean
isBefore el tested = do
  res ← LP.executeScript script
  throwLeft $ J.decodeJson res
  where
  script =
    { script: beforeScript
    , args: [ LT.encodeElement el, LT.encodeElement tested ]
    }

isAfter ∷ ∀ r. LT.Element → LT.Element → R.Run (lunapark ∷ LP.LUNAPARK, except ∷ RE.EXCEPT LE.Error|r) Boolean
isAfter el tested = do
  res ← LP.executeScript script
  throwLeft $ J.decodeJson res
  where
  script =
    { script: beforeScript
    , args: [ LT.encodeElement tested, LT.encodeElement el ]
    }

beforeScript ∷ String
beforeScript = """
  var getParents = function(e, acc) {
    if (typeof acc === "undefined") return getParents(e, [e]);
    var p = e.parentNode;
    if (p == document || p == null) return acc;
    acc.unshift(p);
    return getParents(p, acc);
  };
  var mostCommonArr = function(ls, rs, res) {
    if (typeof res === "undefined") return mostCommonArr(ls, rs, []);
    if (ls.length == 0 || rs.length == 0) return res;
    var l = ls[0];
    var r = rs[0];
    if (l == r) {
      res.push(l);
      return mostCommonArr(ls.slice(1), rs.slice(1), res);
    } else {
      return res;
    }
  };

  var l = arguments[0];
  var r = arguments[1];
  // The left and the right are the same -->
  // this is debatable, but I think it's both after and before
  if (r == l) return true;

  var ls = getParents(l);
  var rs = getParents(r);
  var commons = mostCommonArr(ls, rs);
  // The left is parent of the right --> it's going before
  if (commons.indexOf(l) != -1) return true;
  // The right is parent of the left --> it's going after
  if (commons.indexOf(r) != -1) return false;

  var lastCommon = ls[commons.length];
  var lDistinct = ls.slice(commons.length)[0];
  var rDistinct = rs.slice(commons.length)[0];
  var siblings = lastCommon.children;
  var lIx = 0;
  var rIx = 0;
  var i = 0;
  for (var i = 0; i < siblings.length; i++) {
    if (siblings[i] == lDistinct) lIx = i;
    if (siblings[i] == rDistinct) rIx = i;
  }
  return lIx <= rIx;
"""
