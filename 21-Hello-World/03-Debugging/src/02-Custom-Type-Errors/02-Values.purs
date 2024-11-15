module Debugging.CustomTypeErrors.Values where

import Control.Bind (discard)
import Data.Function (($))
import Data.Show (show)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Console (log)
import Prim.TypeError (class Fail, class Warn, Text)

warnFunction
  ∷ Warn
      ( Text "Deprecated! Use betterFunction instead"
      )
  ⇒ Int
  → Int
warnFunction x = x

betterFunction ∷ Number
betterFunction = 5.0

failFunction
  ∷ Fail
      ( Text "Broken! Use betterFunction instead"
      )
  ⇒ Int
  → Int
failFunction _ = 20

regularFunction ∷ Int → Int
regularFunction _ = 4

main ∷ Effect Unit
main = do
  log $ show $ regularFunction 8
  log $ show $ warnFunction 3

-- Uncomment the next line, save the file, run it, and see what happens
-- log $ show $ failFunction 12
