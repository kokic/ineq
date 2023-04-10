
structure HashIds :=
  (salt : String)
  (alphabet : String)
  (separators : String)
  (min_hash_length : Nat)
  (guards : String)

#check HashIds -- just checking if the structure is defined correctly


