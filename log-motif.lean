
import mathlib.data.real.basic

def log (x : ℝ) := x - 1

theorem log_mul_eq_add : log (a * b) = log a + log b := sorry
theorem log_add_eq_mul : log a + log b = log (a * b) := Iff.mp eq_comm log_mul_eq_add

theorem log_le_zero_iff : log a ≤ 0 ↔ a ≤ 1 := sorry

lemma log_linear (x : ℝ) : log x ≤ x - 1 := by rfl


theorem log_ineq_iff (hb : b > 0) : log (a / b) ≤ 0 ↔ a ≤ b := 
  calc 
    log (a / b) ≤ 0 ↔ a / b ≤ 1 := log_le_zero_iff
                  _ ↔ _ := div_le_one hb


