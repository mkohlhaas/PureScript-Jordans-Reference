module ComputingWithMonads.MonadTell where

import Prelude

import Control.Monad.Writer (Writer, runWriter)
import Control.Monad.Writer.Class (tell)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

type Output = Int
type OtherUsefulData = String

main ∷ Effect Unit
main = case runWriter writeStuff of
  Tuple output otherUsefulData → do
    log $ "Computation's output: " <> show output
    log $ "Other useful data:    " <> otherUsefulData

-- WriterT w                        a
-- WriterT String          Identity Int
writeStuff ∷ Writer OtherUsefulData Output
writeStuff = do
  tell "first string! "
  -- some computation happens here
  tell "second string! "
  -- some computation happens here
  tell "third string!"

  pure 5 -- final output of using MonadTell
