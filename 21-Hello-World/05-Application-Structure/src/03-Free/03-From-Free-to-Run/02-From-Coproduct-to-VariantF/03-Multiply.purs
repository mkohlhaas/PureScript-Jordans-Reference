module Free.ExpressionBased.VariantF.Multiply where

import Prelude hiding (add)

import Data.Functor.Variant (VariantF, inj, on)
import Effect (Effect)
import Effect.Console (log)
import Free.ExpressionBased.VariantF.Add
  ( Add
  , VA
  , add
  , addAlgebra

  , example_add
  , example_value_add
  , vaAlgebraComposed
  )
import Free.ExpressionBased.VariantF.Value
  ( Expression(..)
  , Value
  , eval
  , example_value

  , value
  , valueAlgebra
  )
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

-- data stuff
data MultiplyF e = MultiplyF e e

derive instance Functor MultiplyF

-- VariantF stuff
type Multiply r = (multiply ∷ MultiplyF | r)

multiplySymbol ∷ Proxy "multiply"
multiplySymbol = Proxy

multiply
  ∷ ∀ r
  . Expression (VariantF (Multiply + r))
  → Expression (VariantF (Multiply + r))
  → Expression (VariantF (Multiply + r))
multiply x y = In $ inj multiplySymbol (MultiplyF x y)

-- eval stuff
multiplyAlgebra
  ∷ ∀ r
  . (VariantF r Int → Int)
  → (VariantF (Multiply + r) Int → Int)
multiplyAlgebra =
  on multiplySymbol (\(MultiplyF x y) → x * y)

-- Composition: Exclude Add - Only Value & Multiply
vmAlgebra
  ∷ ∀ r
  . (VariantF r Int → Int)
  → (VariantF (Value + Multiply + r) Int → Int)
vmAlgebra =
  valueAlgebra >>> multiplyAlgebra

example_value_multiply ∷ ∀ r. Expression (VariantF (Value + Multiply + r))
example_value_multiply =
  multiply (value 5) (multiply (value 3) (value 6))

-- Composition: Value, Add, & Multiply
vamAlgebraComposed
  ∷ ∀ r
  . (VariantF r Int → Int)
  → (VariantF (Value + Add + Multiply + r) Int → Int)
vamAlgebraComposed =
  valueAlgebra >>> addAlgebra >>> multiplyAlgebra

-- This approach is not as refactor-resistant
-- to the algebra immediately above
vamAlgebraShorter
  ∷ ∀ r
  . (VariantF r Int → Int)
  → (VariantF (VA + Multiply + r) Int → Int)
vamAlgebraShorter = vaAlgebraComposed >>> multiplyAlgebra

example_value_add_multiply ∷ ∀ r. Expression (VariantF (VA + Multiply + r))
example_value_add_multiply =
  add
    ( multiply
        ( add
            example_value
            (value 8)
        )
        ( multiply
            example_value_multiply
            example_add
        )
    )
    example_value_add

main ∷ Effect Unit
main = do
  log $ show $ eval vmAlgebra example_value_multiply
  log $ show $ eval vamAlgebraComposed example_value_add_multiply
  log $ show $ eval vamAlgebraShorter example_value_add_multiply
