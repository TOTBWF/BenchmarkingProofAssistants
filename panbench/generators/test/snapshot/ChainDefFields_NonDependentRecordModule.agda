module ChainDefFields_NonDependentRecordModule where

open import Agda.Builtin.Nat

record Dummy₁ : Set where
  constructor Const₁
  fields
    f₁ : Nat

record Dummy₂ : Set where
  constructor Const₂
  fields
    f₂ : Nat

record Dummy₃ : Set where
  constructor Const₃
  fields
    f₃ : Nat

record Dummy₄ : Set where
  constructor Const₄
  fields
    f₄ : Nat

record Dummy₅ : Set where
  constructor Const₅
  fields
    f₅ : Nat

example : Dummy₅
example = Const₅ 1
