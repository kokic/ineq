

-- p-adic of a nat

structure AdicNumberSeries :=
  adic : Nat
  data : List Nat
deriving Repr

namespace AdicNumberSeries

partial def fromNatAux (adic residue power : Nat) 
                       (data : List Nat) := 
  match residue with
    | 0 => data
    | r => 
      let base := adic ^ (power - 1)
      let aᵢ := r % (adic ^ power) / base
      let term := aᵢ * base
      fromNatAux adic (r - term) (power + 1) (data ++ [aᵢ])

def fromNat (adic n : Nat) : AdicNumberSeries := ⟨adic, fromNatAux adic n 1 []⟩


def toNat (series : AdicNumberSeries) :=
  let rec aux : List Nat → Nat → Nat
    | [], _ => 0
    | x :: xs, n => x * series.adic ^ n + aux xs (n + 1)
  aux series.data 0


-- #eval toNat 
-- #eval toNat (fromNat 5 7)



-- √n i.e. x s.t. x² = n


-- def sqrtByEnumAux (n adic power sum : Nat) := Id.run do
--   let mut coeff := 0
--   for i in [ :adic ] do
--     let approx := sum + i * adic ^ power
--     let higher := adic ^ (power + 1)
--     if approx ^ 2 % higher == n then 
--     coeff := i
--   return coeff

-- def sqrtByEnum (adic n : Nat) := n



end AdicNumberSeries

