module Syntax.Modification.MonadLikeTypeClasses
  ( class IxFunctor
  , imap
  , map
  , class IxApply
  , iapply
  , apply
  , class IxApplicative
  , ipure
  , pure
  , class IxBind
  , ibind
  , bind
  , class IxMonad
  , Box(..)
  ) where

import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Unit (Unit)

-- Given a data type with instances for the IndexedMonad type class
-- hierarchy (type class instances are below each type class)
data Box ∷ ∀ k. k → k → Type → Type
data Box phantomInput phantomOutput storedValue = Box storedValue

instance (Show a) ⇒ Show (Box x x a) where
  show (Box a) = "Box(" <> show a <> ")"

-- Requirement 1: type classes that are similar to Functor to Monad hierarchy
--  - ado requirements: Functor, Apply, and Applicative
--  - do requirements: Functor, Apply, Applicative, Bind, and Monad

class IxFunctor ∷ ∀ k. (k → k → Type → Type) → Constraint
class IxFunctor f where
  imap ∷ ∀ a b x. (a → b) → f x x a → f x x b

instance IxFunctor Box where
  imap ∷ ∀ a b x. (a → b) → Box x x a → Box x x b
  imap f (Box a) = Box (f a)

class IxApply ∷ ∀ k. (k → k → Type → Type) → Constraint
class (IxFunctor f) ⇐ IxApply f where
  iapply ∷ ∀ a b x y z. f x y (a → b) → f y z a → f x z b

instance IxApply Box where
  iapply ∷ ∀ a b x y z. Box x y (a → b) → Box y z a → Box x z b
  iapply (Box f) (Box a) = Box (f a)

class IxApplicative ∷ ∀ k. (k → k → Type → Type) → Constraint
class (IxApply f) ⇐ IxApplicative f where
  ipure ∷ ∀ a x. a → f x x a

instance IxApplicative Box where
  ipure ∷ ∀ a x. a → Box x x a
  ipure a = Box a

class IxBind ∷ ∀ k. (k → k → Type → Type) → Constraint
class (IxApply m) ⇐ IxBind m where
  ibind ∷ ∀ a b x y z. m x y a → (a → m y z b) → m x z b

instance IxBind Box where
  ibind ∷ ∀ a b x y z. Box x y a → (a → Box y z b) → Box x z b
  ibind (Box a) f =
    -- `f a` produces a value with the type, `Box y z b`, which is
    -- not the return type of this function, `Box x z b`.
    --
    -- So, we can either `unsafeCoerce` the result of `f a` or just
    -- rewrap the 'b' value in a new Box. We've chosen to take the
    -- latter option here for simplicity.
    case f a of Box b → Box b

class IxMonad ∷ ∀ k. (k → k → Type → Type) → Constraint
class (IxApplicative m, IxBind m) ⇐ IxMonad m

instance IxMonad Box

-- Requirement 2: define functions whose names correspond to the ones used
-- in the regular type classes: `map`, `apply`, 'pure', 'bind', and
-- 'discard' (for when bind returns 'unit')
map ∷ ∀ f a b x. IxFunctor f ⇒ (a → b) → f x x a → f x x b
map = imap

apply ∷ ∀ f a b x y z. IxApply f ⇒ f x y (a → b) → f y z a → f x z b
apply = iapply

pure ∷ ∀ f a x. IxApplicative f ⇒ a → f x x a
pure = ipure

bind ∷ ∀ m a b x y z. IxBind m ⇒ m x y a → (a → m y z b) → m x z b
bind = ibind

discard ∷ ∀ a x y z m. IxBind m ⇒ m x y a → (a → m y z Unit) → m x z Unit
discard = ibind
