
import Mathlib.Data.Real.Basic
import Mathlib.Analysis.SpecialFunctions.Log.Basic


-- #check Real.log 1

open Real

lemma log_ineq_iff (a b : ℝ) {ha : a > 0} {hb : b > 0} : 
    log (a / b) ≤ 0 ↔ a ≤ b := by 
  rw [log_nonpos_iff (div_pos ha hb), div_le_one hb] 
  -- simp [log_nonpos_iff, div_pos ha hb, div_le_one hb] 
  -- calc 
  --   _ ↔ a / b ≤ 1 := Real.log_nonpos_iff (div_pos ha hb)
  --   _ ↔ _ := div_le_one hb


-- lemma sq_ne_zero {a : ℝ} (h : a ≠ 0) : a^2 ≠ 0 := pow_ne_zero 2 h



-- dangerous
axiom log_mul_axiom {x y : ℝ} : log (x * y) = log x + log y
axiom log_le_sub_one {x : ℝ} : log x ≤ x - 1


lemma pos_to_nonneg {x : ℝ} (h : x > 0) : x ≥ 0 := le_iff_lt_or_eq.mpr (Or.inl h)

-- bagi i.e. binary arith mean - geo mean ineq


lemma sq_sum_pos {a b : ℝ} (ha : a > 0) (hb : b > 0) : 
    a^2 + b^2 > 0 := by simp [add_pos, sq_pos_of_pos ha, sq_pos_of_pos hb]


#check zero_lt_iff
-- #check pos_iff_ne_zero

-- lemma sq_ne_zero {a : ℝ} (h : a ≠ 0) : a^2 ≠ 0 := pow_ne_zero 2 h

lemma sq_sum_ne_zero {a b : ℝ} (ha : a > 0) (hb : b > 0) : 
    a^2 + b^2 ≠ 0 := ne_of_gt (sq_sum_pos ha hb)


-- by simp [pos_iff_ne_zero, sq_sum_pos ha hb]


-- almost id
lemma bagi.factor_pos {a b : ℝ} (ha : a > 0) (hb : b > 0) : 
    2 * a^2 / (a^2 + b^2) > 0 ∧ 2 * b^2 / (a^2 + b^2) > 0 :=
  have suba : 2 * a^2 > 0 := by simp [mul_pos, ha]
  have subb : 2 * b^2 > 0 := by simp [mul_pos, hb]
  have subs : a^2 + b^2 > 0 := sq_sum_pos ha hb
  ⟨div_pos suba subs, div_pos subb subs⟩



lemma sqrt_sq_pos (h : 0 < x) : sqrt (x ^ 2) = x := by 
  rw [sq, sqrt_eq_iff_mul_self_eq_of_pos h]




lemma bagi.factor_expand_pos {a b : ℝ} (ha : a > 0) (hb : b > 0) : 
    log (2 * a * b / (a^2 + b^2)) = log (sqrt (2 * a^2 / (a^2 + b^2))) 
                                  + log (sqrt (2 * b^2 / (a^2 + b^2))) :=
  let sum := a^2 + b^2
  have pos₁ : 2 * a * b > 0 := by simp [ha, hb, mul_pos]
  have pos₂ : 2 * a * b / sum > 0 := by simp [div_pos, pos₁, sq_sum_pos ha hb]
  have fab_pos : _ := factor_pos ha hb
  have hpos : _ := pos_to_nonneg fab_pos.left
  calc 
    _ = log (sqrt ((2 * a * b / sum)^2)) := by rw [sqrt_sq_pos pos₂]
    _ = log (sqrt ((2 * a^2 / sum) * (2 * b^2 / sum))) := by ring_nf
    _ = log (sqrt (2 * a^2 / sum) * sqrt (2 * b^2 / sum)) := by rw [sqrt_mul hpos]
    _ = _ := log_mul_axiom




lemma bagi.log_elim_pos {a b : ℝ} (ha : a > 0) (hb : b > 0): 
    log (sqrt (2 * a^2 / (a^2 + b^2))) ≤ a^2 / (a^2 + b^2) - 1/2 ∧ 
    log (sqrt (2 * b^2 / (a^2 + b^2))) ≤ b^2 / (a^2 + b^2) - 1/2 :=
  let ⟨pos₁, pos₂⟩ := factor_pos ha hb; by
  apply And.intro
  case left => 
    have elim₁ : log (2 * a^2 / (a^2 + b^2)) ≤ _ := log_le_sub_one
    have elim₂ : _ := div_le_div_of_le zero_le_two elim₁
    calc
      _ = _ := log_sqrt (pos_to_nonneg pos₁)
      _ ≤ _ := elim₂
      _ = _ := by ring
  case right => 
    have elim₁ : log (2 * b^2 / (a^2 + b^2)) ≤ _ := log_le_sub_one
    have elim₂ : _ := div_le_div_of_le zero_le_two elim₁
    calc
      _ = _ := log_sqrt (pos_to_nonneg pos₂)
      _ ≤ _ := elim₂
      _ = _ := by ring



lemma bagi.frac_elim {a b : ℝ} (ha : a > 0) (hb : b > 0): 
    a^2 / (a^2 + b^2) - 1/2 + b^2 / (a^2 + b^2) - 1/2 = 0 := by  
  calc 
    _ = (a^2 + b^2) / (a^2 + b^2) - 1 := by ring
    _ = 0 := by simp [div_self, sq_sum_ne_zero ha hb]






















-- nonneg
lemma mul_nonneg_pos {a b : ℝ} (ha : a ≥ 0) (hb : b > 0) : a * b ≥ 0 := 
  mul_nonneg_iff_left_nonneg_of_pos hb |>.mpr ha

lemma sq_sum_nonneg {a b : ℝ} : a^2 + b^2 ≥ 0 := by 
  simp [add_nonneg, sq_nonneg]

lemma bagi.factor_nonneg {a b : ℝ} (ha : a ≥ 0) (hb : b ≥ 0): 
    2 * a^2 / (a^2 + b^2) ≥ 0 ∧ 2 * b^2 / (a^2 + b^2) ≥ 0 :=
  have suba : 2 * a^2 ≥ 0 := by simp [mul_nonneg_pos, ha]
  have subb : 2 * b^2 ≥ 0 := by simp [mul_nonneg_pos, hb]
  have subs : a^2 + b^2 ≥ 0 := sq_sum_nonneg
  ⟨div_nonneg suba subs, div_nonneg subb subs⟩






-- lemma sqrt_mul_pos (hx : x > 0) (y : ℝ) : 
--     sqrt (x * y) = sqrt x * sqrt y := sorry

-- lemma bagi.factor_expand_pos {a b : ℝ} (ha : a > 0) (hb : b > 0) : 
--     log (2 * a * b / (a^2 + b^2)) = log (sqrt (2 * a^2 / (a^2 + b^2))) 
--                                   + log (sqrt (2 * b^2 / (a^2 + b^2))) :=
  
--   let sum := a^2 + b^2
--   have pos : 2 * a * b / sum > 0 := sorry
--   have fab_pos : _ := factor_pos ha hb
--   have fab_ne : _ := And.intro (sqrt_ne_zero'.mpr fab_pos.left) (sqrt_ne_zero'.mpr fab_pos.right)

--   calc 
--     _ = log (sqrt ((2 * a * b / sum)^2)) := by rw [sqrt_sq pos]
--     _ = log (sqrt ((2 * a^2 / sum) * (2 * b^2 / sum))) := by ring_nf
--     _ = log (sqrt (2 * a^2 / sum) * sqrt (2 * b^2 / sum)) := by rw [sqrt_mul_pos fab_pos.left]
--     _ = _ := log_mul fab_ne.left fab_ne.right



lemma bagi.factor_expand {a b : ℝ} (ha : a ≥ 0) (hb : b ≥ 0) : 
    log (2 * a * b / (a^2 + b^2)) = log (sqrt (2 * a^2 / (a^2 + b^2))) 
                                  + log (sqrt (2 * b^2 / (a^2 + b^2))) :=
  let sum := a^2 + b^2
  have nneg₁ : 2 * a * b ≥ 0 := by simp [ha, hb, mul_nonneg, mul_nonneg_pos]
  have nneg₂ : 2 * a * b / sum ≥ 0 := by simp [div_nonneg, nneg₁, sq_sum_nonneg]
  have fab_nneg : _ := factor_nonneg ha hb
  calc 
    _ = log (sqrt ((2 * a * b / sum)^2)) := by rw [sqrt_sq nneg₂]
    _ = log (sqrt ((2 * a^2 / sum) * (2 * b^2 / sum))) := by ring_nf
    _ = log (sqrt (2 * a^2 / sum) * sqrt (2 * b^2 / sum)) := by rw [sqrt_mul fab_nneg.left]
    _ = _ := log_mul_axiom

-- have fab_ne : sqrt (2 * a^2 / (a^2 + b^2)) ≠ 0 ∧ 
--               sqrt (2 * b^2 / (a^2 + b^2)) ≠ 0 := sorry 
-- _ := And.intro (sqrt_ne_zero'.mpr fab_pos.left) (sqrt_ne_zero'.mpr fab_pos.right)
-- log_mul fab_ne.left fab_ne.right







lemma bagi.log_elim {a b : ℝ} (ha : a ≥ 0) (hb : b ≥ 0): 
    log (sqrt (2 * a^2 / (a^2 + b^2))) ≤ a^2 / (a^2 + b^2) - 1/2 ∧ 
    log (sqrt (2 * b^2 / (a^2 + b^2))) ≤ b^2 / (a^2 + b^2) - 1/2 :=
  let ⟨nneg₁, nneg₂⟩ := factor_nonneg ha hb; by
  apply And.intro
  case left => 
    have elim₁ : log (2 * a^2 / (a^2 + b^2)) ≤ _ := log_le_sub_one
    have elim₂ : _ := div_le_div_of_le zero_le_two elim₁
    calc
      _ = _ := log_sqrt nneg₁
      _ ≤ _ := elim₂
      _ = _ := by ring
  case right => 
    have elim₁ : log (2 * b^2 / (a^2 + b^2)) ≤ _ := log_le_sub_one
    have elim₂ : _ := div_le_div_of_le zero_le_two elim₁
    calc
      _ = _ := log_sqrt nneg₂
      _ ≤ _ := elim₂
      _ = _ := by ring




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
