
import Mathlib.Data.Real.Basic

lemma sub_sq_nonneg (a b : ℝ) : (a-b)^2 ≥ 0 := 
  by exact sq_nonneg (a-b)

lemma sub_fundamental_ineq (a b : ℝ) : a^2 + b^2 - 2 * a * b ≥ 0 := 
  calc 
    _ = _ := by ring -- linarith
    _ ≥ _ := sub_sq_nonneg a b


-- lemma sub_fundamental_ineq' (a b : ℝ) : a^2 + b^2 - 2 * a * b ≥ 0 := 
--   calc 
--     a^2 + b^2 - 2 * a * b = (a-b)^2 := by ring -- linarith
--     (a-b)^2 ≥ 0 := sub_sq_nonneg a b



lemma add_ge_add {a b c d : ℝ} (h₁ : a ≥ b) (h₂ : c ≥ d) : a + c ≥ b + d :=
  have h₁' : _ := Iff.mp ge_iff_le h₁
  have h₂' : _ := Iff.mp ge_iff_le h₂
  show _ from add_le_add  h₁' h₂'
  -- have rev : b + d ≤ a + c := add_le_add
  -- show _ from rev

lemma fundamental_ineq (a b : ℝ) : a^2 + b^2 ≥ 2*a*b :=
  have hs : 0 ≤ a^2 + b^2 - 2*a*b := sub_fundamental_ineq a b
  calc
    -- 0 + 2 * a * b ≤ (a^2 + b^2 - 2 * a * b) + 2 * a * b := add_le_add hs (le_refl _)
     a^2 + b^2 = (a^2 + b^2 - 2 * a * b) + 2 * a * b := by ring
             _ ≥ 0 + 2 * a * b := add_ge_add hs (le_refl _) -- linarith
             _ = 2 * a * b := by ring 

    -- a^2 + b^2 = a^2 + b^2 - 2* a * b + 2* a *b := by ring 
    --         _ = 2* a *b + 0                 := by ring



#eval println! "hello ℓ₁"

