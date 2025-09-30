module Fields_NonDependentRecordModule where

open import Agda.Builtin.Nat

record Cap_X : Set where
  constructor Const
  fields
    f₁ : Nat
    f₂ : Nat
    f₃ : Nat
    f₄ : Nat
    f₅ : Nat

example : Cap_X
example = Const 1 1 1 1 1
