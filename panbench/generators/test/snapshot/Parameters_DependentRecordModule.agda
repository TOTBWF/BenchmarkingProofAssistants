module Parameters_DependentRecordModule where

open import Agda.Builtin.Nat

record X (f₁ : Nat) (f₂ : Nat) (f₃ : Nat) (f₄ : Nat) (f₅ : Nat) : Set where
  constructor Const
  fields
    sums : Nat

example : X 1 2 3 4 5
example = Const (1 + 2 + 3 + 4 + 5)
