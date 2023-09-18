
-- import algorithm.Diophantus.ContinuedFrac

-- #check ContinuedFrac

-- Quadratic

namespace PellEquation


-- def aux (x : Float) :=
  -- 1 / (x - x.floor)

def quadraticContinuedFracFinite (d n : Nat) := Id.run do
  let mut xs : List Nat := []
  let mut approx := Float.sqrt d.toFloat
  for _ in [:n] do
    let cᵢ := approx.floor
    xs := xs ++ [cᵢ.toUInt32.toNat]
    approx := 1 / (approx - cᵢ)
  xs



-- #check coeffByEuclidAux
-- #eval quadraticContinuedFracFinite 7 10



-- #eval 
-- #eval (1 / (sq5 - sq5.floor)).floor


