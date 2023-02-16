
import Mathlib.Data.Real.Basic


lemma sub_sq_nonneg (a b : ℝ) : (a-b)^2 ≥ 0 := 
  by exact sq_nonneg (a-b)

lemma sub_fundamental_ineq (a b : ℝ) : a^2 + b^2 - 2*a*b ≥ 0 := 
  calc 
    _ = _ := by ring -- linarith
    _ ≥ _ := sub_sq_nonneg a b



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


lemma sqrt_nonneg (a : ℝ) (pa : a ≥ 0) : a^(1/2) ≥ 0 := sorry
-- lemma sqrt_merge (a b : ℝ) : a^(1/2) * b^(1/2) = (a*b)^(1/2) := sorry
-- lemma sqrt_expand (a b : ℝ) : (a*b)^(1/2) = a^(1/2) * b^(1/2) := sorry

lemma bi_sqrt_nonneg (a b : ℝ) (ha : a ≥ 0) (hb : b ≥ 0) : (a*b)^(1/2) ≥ 0 := 
  have hab : _ := mul_nonneg ha hb 
  show _ from sqrt_nonneg (a * b) hab

-- example (a b : ℝ) (pa : a ≥ 0) (pb : b ≥ 0) : 
--   a + b ≥ 2*(a*b)^(1/2) := 
--   calc 
--     a^2 + b^2 - 2* a *b = 
--     a + b - 2*(a*b)^(1/2) := sqrt_nonneg a ∧ sqrt_nonneg b

lemma sqrt_sq (a : ℝ) (pa : a ≥ 0) : a = (a^(1/2))^2 := sorry
lemma le_sqrt_sq (a : ℝ) (pa : a ≥ 0) : a ≤ (a^(1/2))^2 := sorry


theorem bi_arith_geom_ineq (a b : ℝ) 
    (pa : a ≥ 0) (pb : b ≥ 0) : a + b ≥ 2*a^(1/2)*b^(1/2) :=
  have ha : _ := sqrt_sq a pa
  have hb : _ := sqrt_sq b pb
  have hal : _ := le_sqrt_sq a pa
  have hbl : _ := le_sqrt_sq b pb
  have hs : _ := And.intro ha hb
  have heq : _ := Iff.mpr (add_eq_add_iff_eq_and_eq hal hbl) hs
  calc 
    a + b = (a^(1/2))^2 + (b^(1/2))^2 := heq
        _ ≥ 2*a^(1/2)*b^(1/2) := fundamental_ineq _ _



#eval println! "hello ℓ₁"

