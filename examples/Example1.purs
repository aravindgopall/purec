module Example1 where

import Effect
import Prelude

import Effect.Console as Console

foreign import runGC :: Effect Unit
foreign import xpure :: ∀ a. a -> Effect a
foreign import xbind :: ∀ a b. Effect a -> (a -> Effect b) -> Effect b
foreign import xdiscard :: ∀ a b. Effect a -> (Unit -> Effect b) -> Effect b

xmain :: Effect Unit
xmain = do
  pure unit
  runGC

  where
  bind = xbind
  pure = xpure
  discard = xdiscard

main :: Effect Unit
main = do
  pure unit
  foo <- pure "quark"
  runGC
  xmain
  runGC
  Console.log ("test: " <> foo)

  where
  bind = xbind
  discard = xdiscard
  pure = xpure
