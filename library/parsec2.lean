
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



def to_string (p : Parser Char) := p >>= (pure ·.toString)
  -- Parser.mk λ s => match p.parse s with
  --   | some ⟨a, r⟩ => some (a.toString, r)
  --   | _ => none

-- def follow {α β : Type} (p1 : Parser α) (p2 : Parser β) : Parser (α × β) :=
def follow (p : Parser α) (p' : Parser β) := p >>= λ a => p' >>= λ b => pure (a, b)

-- #eval (follow (char 'x') (char 'd')).parse "xdd"


def move (p : Parser α) (p' : Parser β) := follow p p' >>= (pure ·.1)

def map2 (p : Parser (α × β)) (f : α × β → γ) := p >>= (· |> f |> pure)


def either (p1 p2 : Parser α) : Parser α :=
  Parser.mk λ s => match p1.parse s with
    | some x => some x
    | none => p2.parse s



-- def many_aux (p : Parser α) (data: List α) (residue : String) : Parser (List α) := 
--   Parser.mk λ s => 
--     have h1 : List α × String := (data, residue)
--     have h2 : List α × String → Option (List α × String) := λ x => some x
--     match p.parse s with
--       | some ⟨a, r⟩ => 
--         have h3 : List α × String := (data ++ [ a ], r)
--         have h4 : List α × String → Option (List α × String) := λ x => many_aux p x.1 x.2 |>.parse r
--         h4 h3
--       | _ => h2 h1
-- termination_by (measureOfList α) data residue => (data, residue)
-- termination_by _ data residue => (data, residue)
-- decreasing_by simp


partial def many_aux (p : Parser α) (data: List α) (residue : String) : List α × String := 
  match p.parse residue with
    | some ⟨a, r⟩ => many_aux p (data ++ [a]) r
    | _ => (data, residue)

def many (p : Parser α) : Parser (List α) := Parser.mk (λ s => many_aux p [] s)



def asterisk (p : Parser String) : Parser String := many p >>= (· |> appends |> pure)
  where appends : (List String -> String) := (·.foldl String.append "")


-- #eval char 'a' |> to_string |> asterisk |>.parse "aabb"

-- def asterisk_aux (p : Parser String) (data: String) (residue : String) : Parser String := 
--   Parser.mk λ s => match p.parse s with
--     | [(a, r)] => asterisk_aux (to_string (char 'a')) (data ++ a) r |>.parse s
--     | _ => [(data, residue)]







