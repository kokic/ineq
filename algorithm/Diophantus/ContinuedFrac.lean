

structure Mat2x2 (α : Type u) :=
  a₁₁ : α 
  a₁₂ : α 
  a₂₁ : α 
  a₂₂ : α
deriving Repr 



-- class ConcreteMat.Mul (Mat : Type) where
  -- mul : Mat → Mat → Mat

class AddMul (α : Type u) where
  add : α → α → α
  mul : α → α → α 
-- export RingOperator (add mul)


instance : AddMul Nat := ⟨.add, .mul⟩
instance : AddMul Int := ⟨.add, .mul⟩

def Mat2x2.mul [AddMul α] : Mat2x2 α → Mat2x2 α → Mat2x2 α := 
  let (add, mul) := (AddMul.add, AddMul.mul)
  λ A B => Mat2x2.mk 
    (add (mul A.a₁₁ B.a₁₁) (mul A.a₁₂ B.a₂₁))
    (add (mul A.a₁₁ B.a₁₂) (mul A.a₁₂ B.a₂₂))
    (add (mul A.a₂₁ B.a₁₁) (mul A.a₂₂ B.a₂₁))
    (add (mul A.a₂₁ B.a₁₂) (mul A.a₂₂ B.a₂₂))

-- def ConcreteMat.mul (A B : Mat2x2 α) := 



-- instance : ConcreteMat.Mul (Mat2x2 Nat) := ⟨λ A B => Mat2x2.mul A B Nat.add Nat.mul⟩
-- instance : ConcreteMat.Mul (Mat2x2 Int) := ⟨λ A B => Mat2x2.mul A B Int.add Int.mul⟩




def A := Mat2x2.mk 1 2 3 4

#eval A.mul A


-- #check (Mat2x2 Nat).mul⟩

-- def regularContinuedFrac

