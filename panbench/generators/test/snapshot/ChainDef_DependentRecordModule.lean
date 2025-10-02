
structure Dummy₁ : Type
  Const₁ ::
  f₁ : Nat
  open Dummy₁

structure Dummy₂ : Type
  Const₂ ::
  f₂ : Dummy₁
  open Dummy₂

structure Dummy₃ : Type
  Const₃ ::
  f₃ : Dummy₂
  open Dummy₃

structure Dummy₄ : Type
  Const₄ ::
  f₄ : Dummy₃
  open Dummy₄

structure Dummy₅ : Type
  Const₅ ::
  f₅ : Dummy₄
  open Dummy₅

def example : Dummy₅ := (Const₅ (Const₄ (Const₃ (Const₂ (Const₁ 10)))))