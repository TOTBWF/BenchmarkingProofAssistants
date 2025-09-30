module ChainDef_DependentRecordModule where

record Dummy₁ : Set where
  constructor Const₁
  fields
    f₁ : Nat

record Dummy₂ : Set where
  constructor Const₂
  fields
    f₂ : Dummy₁

record Dummy₃ : Set where
  constructor Const₃
  fields
    f₃ : Dummy₂

record Dummy₄ : Set where
  constructor Const₄
  fields
    f₄ : Dummy₃

record Dummy₅ : Set where
  constructor Const₅
  fields
    f₅ : Dummy₄

example : Dummy₅
example = (Const₅ (Const₄ (Const₃ (Const₂ (Const₁ 10)))))
