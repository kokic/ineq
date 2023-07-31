
import Mathlib.Data.Real.Basic
import Mathlib.Analysis.SpecialFunctions.Log.Basic


-- #check Real.log 1

#check Real.log_mul
#check Real.log_pow
#check Real.log_sqrt
#check Real.log_le_sub_one_of_pos



open Real

lemma log_ineq_iff (a b : ℝ) {ha : a > 0} {hb : b > 0} : 
    log (a / b) ≤ 0 ↔ a ≤ b := by 
  rw [log_nonpos_iff (div_pos ha hb), div_le_one hb] 
  -- simp [log_nonpos_iff, div_pos ha hb, div_le_one hb] 
  -- calc 
  --   _ ↔ a / b ≤ 1 := Real.log_nonpos_iff (div_pos ha hb)
  --   _ ↔ _ := div_le_one hb



theorem sinh_log {x : ℝ} (hx : 0 < x) : sinh (log x) = (x - x⁻¹) / 2 := by
  rw [sinh_eq, exp_neg, exp_log hx]

-- lemma ann : log a = log (a - b + b) := by ring

lemma sq_ne_zero {a : ℝ} (h : a ≠ 0) : a^2 ≠ 0 := pow_ne_zero 2 h

#check sq_nonneg
#check add_pos

-- #check add_pos'


-- bagi i.e. binary arith mean - geo mean ineq



lemma mul_nonneg_pos {a b : ℝ} (ha : a ≥ 0) (hb : b > 0) : a * b ≥ 0 := 
  mul_nonneg_iff_left_nonneg_of_pos hb |>.mpr ha

-- lemma bagi.factor_nonneg {a b : ℝ} (ha : a ≥ 0) (hb : b ≥ 0): 
--     2 * a / (a^2 + b^2) ≥ 0 ∧ 2 * b / (a^2 + b^2) ≥ 0 :=
--   have suba : 2 * a ≥ 0 := by simp [mul_nonneg_pos, ha]
--   have subb : 2 * b ≥ 0 := by simp [mul_nonneg_pos, hb]
--   have subs : a^2 + b^2 ≥ 0 := by simp [add_nonneg, sq_nonneg]
--   ⟨div_nonneg suba subs, div_nonneg subb subs⟩


-- almost id
lemma bagi.factor_pos {a b : ℝ} (ha : a > 0) (hb : b > 0): 
    2 * a^2 / (a^2 + b^2) > 0 ∧ 2 * b^2 / (a^2 + b^2) > 0 :=
  have suba : 2 * a^2 > 0 := by simp [mul_pos, ha]
  have subb : 2 * b^2 > 0 := by simp [mul_pos, hb]
  have subs : a^2 + b^2 > 0 := by 
    simp [add_pos, sq_pos_of_pos ha, sq_pos_of_pos hb]
  ⟨div_pos suba subs, div_pos subb subs⟩





lemma sqrt_mul_pos (hx : x > 0) (y : ℝ) : 
    sqrt (x * y) = sqrt x * sqrt y := sorry
  
-- #check pos_


#check sqrt_mul_pos

lemma bagi.factor_expand {a b : ℝ} (ha : a > 0) (hb : b > 0) : 
    log (2 * a * b / (a^2 + b^2)) = log (sqrt (2 * a^2 / (a^2 + b^2))) 
                                  + log (sqrt (2 * b^2 / (a^2 + b^2))) :=
  
  let sqsum := a^2 + b^2  
  -- have ann : _ := bagi.factor_pos ha hb |>.left
  have fab_pos : _ := factor_pos ha hb
  have fab_ne : _ := And.intro
    (sqrt_ne_zero'.mpr fab_pos.left)
    (sqrt_ne_zero'.mpr fab_pos.right)

  calc 
    _ = log (2 * a * b / (a^2 + b^2)) := by trivial
    _ = log (sqrt ((2 * a^2 / sqsum) * (2 * b^2 / sqsum))) := sorry
    _ = log (sqrt (2 * a^2 / sqsum) * sqrt (2 * b^2 / sqsum)) := by 
      rw [sqrt_mul_pos fab_pos.left]
    _ = _ := log_mul fab_ne.left fab_ne.right

    -- _ = _ := by rw [log_mul hfa hfb]



-- lemma square_sum_ge_mul (a b : ℝ) : a^2 + b^2 ≥ 2 * a * b := 
  


-- theorem bi_arith_geom_ineq (a b : ℝ) (ha : a > 0) (hb : b > 0) : 
  -- Real.log ( (a*b)^(1/2) / ((a+b)/2) ) ≤ 0 := 


/-

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

-/
