
import mathlib.data.real.basic

lemma sub_sq_nonneg (a b : ℝ) : (a-b)^2 ≥ 0 := 
  by exact sq_nonneg (a-b)

lemma sub_fundamental_ineq (a b : ℝ) : a^2 + b^2 - 2*a*b ≥ 0 := 
  calc 
    _ = _ := by ring
    _ ≥ _ := sub_sq_nonneg a b




lemma add_ge_add {a b c d : ℝ} (h₁ : a ≥ b) (h₂ : c ≥ d) : a + c ≥ b + d :=
  have h₁' : b ≤ a := Iff.mpr ge_iff_le h₁
  have h₂' : d ≤ c := Iff.mpr ge_iff_le h₂
  show _ from add_le_add  h₁' h₂'
  -- have rev : b + d ≤ a + c := add_le_add
  -- show _ from rev

lemma fundamental_ineq (a b : ℝ) : a^2 + b^2 ≥ 2*a*b :=
  have hs : 0 ≤ a^2 + b^2 - 2*a*b := sub_fundamental_ineq a b
  calc
    -- 0 + 2 * a * b ≤ (a^2 + b^2 - 2 * a * b) + 2 * a * b := add_le_add hs (le_refl _)
     a^2 + b^2 = (a^2 + b^2 - 2 * a * b) + 2 * a * b := by ring
             _ ≥ 0 + 2 * a * b := add_ge_add hs (le_refl _)
             _ = 2 * a * b := by ring

    -- a^2 + b^2 = a^2 + b^2 - 2* a * b + 2* a *b := by ring 
    --         _ = 2* a *b + 0                 := by ring
    


lemma sqrt_nonneg (a : ℝ) (pa : a ≥ 0) : a^(1/2) ≥ 0 := sorry
-- lemma sqrt_merge (a b : ℝ) : a^(1/2) * b^(1/2) = (a*b)^(1/2) := sorry
-- lemma sqrt_expand (a b : ℝ) : (a*b)^(1/2) = a^(1/2) * b^(1/2) := sorry

lemma bi_sqrt_nonneg (a b : ℝ) (ha : a ≥ 0) (hb : b ≥ 0) : (a*b)^(1/2) ≥ 0 := 
  have hab : a*b ≥ 0 := mul_nonneg ha hb 
  show _ from sqrt_nonneg (a * b) hab

-- example (a b : ℝ) (pa : a ≥ 0) (pb : b ≥ 0) : 
--   a + b ≥ 2*(a*b)^(1/2) := 
--   calc 
--     a^2 + b^2 - 2* a *b = 
--     a + b - 2*(a*b)^(1/2) := sqrt_nonneg a ∧ sqrt_nonneg b


theorem bi_arith_geom_ineq (a b : ℝ)
  (pa : a ≥ 0) (pb : b ≥ 0) : (a + b) / 2 ≥ (a*b)^(1/2) := sorry
  -- have hf : a^2 + b^2 ≥ 2*a*b := fundamental_ineq
  
  
--   have eq1 : (a + b) / 2 * 2 >= (a*b)^(1/2) * 2 := mul_le_mul
--   have eq2 : a^2 + b^2 ≥ 2* a *b :=  
--   show eq2 from fundamental_ineq

#eval println! "hello ℓ₁"

