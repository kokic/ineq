
-- single branching version

structure Parser (α : Type) :=
  parse : String → Option (α × String)

instance : Monad Parser := {
  -- pure := λ a => Parser.mk λ s => some (a, s) 
  -- pure := Parser.mk ∘ λ a => some ∘ Prod.mk a
  pure := Parser.mk ∘ (some ∘ Prod.mk ·)
  bind := λ p f => Parser.mk λ s =>
    match p.parse s with
      | some (a, s') => f a |>.parse s'
      | _ => none
}



def satisfy (p : Char → Bool) : Parser Char :=
  Parser.mk λ s =>
    match s.toList with
      | [] => none
      | (h :: t) => if p h then (h, String.mk t) else none
-- #eval satisfy (· == '#') |>.parse "#include"

def char (x : Char) := satisfy (· == x)
-- #eval char 'x' |>.parse "xd"



def to_string (p : Parser Char) := 
  Parser.mk λ s => match p.parse s with
    | some ⟨a, r⟩ => some (a.toString, r)
    | _ => none

def follow {α β : Type} (p1 : Parser α) (p2 : Parser β) : Parser (α × β) :=
  p1 >>= λ a => p2 >>= λ b => pure (a, b)

#eval (follow (char 'x') (char 'd')).parse "xdd"



-- def many_aux (p : Parser α) (data: List α) (residue : String) : Parser (List α) := 
--   Parser.mk λ s => match p.parse s with
--     | [(a, r)] => many_aux p (data ++ [a]) r |>.parse s
--     | _ => [(data, residue)]
-- termination_by _ data residue => [(data, residue)]
-- decreasing_by simp


-- def asterisk_aux (p : Parser String) (data: String) (residue : String) : Parser String := 
--   Parser.mk λ s => match p.parse s with
--     | [(a, r)] => asterisk_aux (to_string (char 'a')) (data ++ a) r |>.parse s
--     | _ => [(data, residue)]

-- def many (p1 : Parser α) : Parser List α := 


