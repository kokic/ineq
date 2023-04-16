

structure Parser (α : Type) :=
  (parse : String → List (α × String))


-- remark: we use list state to represent `ParseState` i.e. 
--     `parse succ   ↔ list not empty`
--     `parse failed ↔ list empty (nil)`
-- and also to store multiple branching of parse.

-- general, we should define a structure to store `ParseState`, 
-- it may be the following in rust: 
--     `Optional := Some _ | None` or `Result := Ok _ | Err _`
-- so, if we choose list type to replace them then
--     `parse succ   ↔ [_, ...] ↔ Some ↔ Ok`
--     `parse failed ↔ []       ↔ None ↔ Err`



def satisfy (p : Char → Bool) : Parser Char :=
  Parser.mk fun s =>
    match s.toList with
      | [] => []
      | (h :: t) => if p h then [(h, String.mk t)] else []
-- #eval satisfy (· == '#') |>.parse "#include"

def char (x : Char) := satisfy (· == x)
-- #eval char 'x' |>.parse "xd"

def to_string (p : Parser Char) := 
  Parser.mk λ s => List.map (λ ⟨a, r⟩ => (a.toString, r)) (p.parse s)

def follow {α β : Type} (p1 : Parser α) (p2 : Parser β) : Parser (α × β) :=
  Parser.mk λ s => 
    List.map 
      (λ ⟨a, r₁⟩ => List.map 
      (λ ⟨b, r₂⟩ => ((a, b), r₂)) 
      (p2.parse r₁)) (p1.parse s)
    |>.join

#eval (follow (char 'x') (char 'd')).parse "xdd"



-- def many_aux (p : Parser α) (data: List α) (residue : String) : Parser (List α) := 
--   Parser.mk λ s => match p.parse s with
--     | [(a, r)] => many_aux p (data ++ [ a ]) r |>.parse s
--     | _ => [(data, residue)]
-- termination_by _ data residue => [(data, residue)]
-- decreasing_by simp

-- def many (p1 : Parser α) : Parser List α := 


