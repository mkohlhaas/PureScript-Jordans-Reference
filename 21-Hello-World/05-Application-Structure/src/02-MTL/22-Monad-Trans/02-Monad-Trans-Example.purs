module ComputingWithMonads.MonadTrans where

import Prelude

import Control.Monad.State (State, runState)
-- State
import Control.Monad.State.Class (class MonadState, get, gets, modify_)
-- Writer
import Control.Monad.Writer.Class (class MonadWriter, tell)
import Control.Monad.Writer.Trans (WriterT, runWriterT)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

program
  ∷ ∀ m
  . MonadState Int m
  ⇒ MonadWriter String m
  ⇒ m String
program = do
  currentState ← get
  tell $ "Current State is now: " <> show currentState
  modify_ (_ + 10)
  gets show

run
  ∷ ∀ a
  . WriterT String (State Int) a
  → Int
  → Tuple (Tuple a String) Int
run function initialState = runState (runWriterT function) initialState

main ∷ Effect Unit
main = case run program 0 of
  Tuple (Tuple output nonOutputData) nextState → do
    log $ "Finished!"
    log $ "(Computation) final output: " <> show output
    log $ "(Writer)   non-output data: " <> show nonOutputData
    log $ "(State)         next state: " <> show nextState

    let (Tuple (Tuple o _) _) = run program 8
    log $ "Using pattern matching to get the computation's output: " <> show o
