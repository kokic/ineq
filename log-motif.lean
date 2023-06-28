
import Mathlib.Data.Real.Basic


def log (x : ℝ) := x - 1



-- #check 

theorem log_mul_eq_add : log (a * b) = log a + log b := sorry
theorem log_add_eq_mul : log a + log b = log (a * b) := Iff.mp eq_comm log_mul_eq_add

-- theorem log_pm (a n : ℝ) : log (a ^ n) = n * log a := sorry

theorem log_le_zero_iff : log a ≤ 0 ↔ a ≤ 1 := sorry

lemma log_linear (x : ℝ) : log x ≤ x - 1 := by rfl


theorem log_ineq_iff (a b : ℝ) (hb : b > 0) : log (a / b) ≤ 0 ↔ b ≥ a := 
  calc 
    _ ↔ a / b ≤ 1 := log_le_zero_iff
    _ ↔ _ := div_le_one hb

-- theorem log_


lemma sqrt_self (a : ℝ) : a = a^(1/2) * a^(1/2) := sorry
lemma sqrt_decompose (a b : ℝ) : (a*b)^(1/2) = a^(1/2) * b^(1/2) := sorry

lemma log_sqrt (a : ℝ) : log (a^(1/2)) = (1/2) * log a := sorry
lemma add_map (a b : ℝ) (f : ℝ -> ℝ) : a + b = f a + f b := sorry


lemma log_add_sqrt (a b : ℝ): log (a^(1/2)) + log (b^(1/2)) = (1/2) * log a + (1/2) * log b 
  := sorry



theorem log_bi_arith_geom_ineq (a b : ℝ) (ha : a > 0) (hb : b > 0) : 
  log ( (a*b)^(1/2) / ((a+b)/2) ) ≤ 0 := 
  have hale : _ := log_linear (2 * a/(a+b))
  have hble : _ := log_linear (2 * b/(a+b))
  have hline : log (2 * a/(a+b)) + log (2 * b/(a+b)) ≤ (2 * a/(a+b) - 1) + (2 * b/(a+b) - 1) 
    := add_le_add hale hble
  have hsum : a + b ≠ 0 := sorry
  have hp : _ := sorry
  have hp' : _ := sorry
  calc 
      _ = _ := sorry
      _ = log ((2 * a/(a+b))^(1/2)) + log ((2*b/(a+b))^(1/2)) := log_mul_eq_add
      _ = (1/2) * log (2 * a/(a+b)) + (1/2) * log (2*b/(a+b)) := log_add_sqrt _ _
      _ = (1/2) * ( log (2 * a/(a+b)) + log (2 * b/(a+b)) ) := by ring
      _ ≤ (1/2) * (2 * a/(a+b) - 1 + (2 * b/(a+b) - 1)) := mul_le_mul le_rfl hline hp hp'
      _ = (a + b) / (a + b) - 1 := by ring
      _ = 1 - 1 := by rw [div_self hsum]
      _ = 0 := by simp

theorem bi_arith_geom_ineq (a b : ℝ)
  (ha : a > 0) (hb : b > 0) : (a + b) / 2 ≥ (a*b)^(1/2) := 
  have hsum : a + b > 0 := add_pos ha hb
  have h2 : 2 > 0 := by simp
  have hbot : (a + b) / 2 > 0 := Iff.mp gt_iff_lt (div_pos hsum h2)
  have hlog : _ := log_bi_arith_geom_ineq a b ha hb
  show _ from Iff.mp (log_ineq_iff _ _ hbot) hlog



  -- calc 
    
  -- have heq1 : _ := sqrt_decompose a b
--   have heq2 : (a * b)^(1/2) / ((a+b)/2) = a^(1/2)*b^(1/2) / ((a+b)/2) := sorry
--   have heq3 : a^(1/2) * b^(1/2) / ((a+b)/2) = ((2 * a/(a+b))^(1/2)) * ((2*b/(a+b))^(1/2)) := sorry
  
--   have hle1 _ := log_linear (2*a/(a+b))
--   -- have hle2 : (1/2) * log (2*a/(a+b)) ≤ (1/2) * (2 * a / (a + b) - 1) := mul_le_mul (le_refl _) hle1
--   calc
--     _ = log _ := congrArg log heq3
--     _ = log ((2 * a/(a+b))^(1/2)) + log ((2*a/(a+b))^(1/2)) := log_mul_eq_add
--     _ = (1/2) * log (2 * a/(a+b)) + (1/2) * log (2*b/(a+b)) := sorry
--     _ ≤ 0 := sorry

