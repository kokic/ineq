

structure Parser (α : Type) :=
  (parse : String → List (α × String))





def satisfy (p : Char → Bool) : Parser Char :=
  Parser.mk fun s =>
    match s.toList with
      | [] => []
      | (h :: t) => if p h then [(h, String.mk t)] else []
-- #eval satisfy (· == '#') |>.parse "#include"

def char (x : Char) := satisfy (· == x)
-- #eval char 'x' |>.parse "xd"


def follow {α β : Type} (p1 : Parser α) (p2 : Parser β) : Parser (α × β) :=
  Parser.mk λ s => 
    List.map 
      (λ ⟨a, r₁⟩ => List.map 
        (λ ⟨b, r₂⟩ => ((a, b), r₂)) 
        (p2.parse r₁)) (p1.parse s)
      |>.join

#eval (follow (char 'x') (char 'd')).parse "xdd"

