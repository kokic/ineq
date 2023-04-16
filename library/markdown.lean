

-- WIP. 

import library.parsec


def heading := char '#'

def paragraph := satisfy_alt (λ _ => true) 
  |>.plus |>.map (s!"<p>{·}</p>")

-- def newline := string "\n\n"

def line (s : String) := 
  let s := s.replace "\n" ""
  match paragraph.parse s with
    | some x => x.fst
    | none => s

def translate (s : String) := 
  let xs := s.splitOn s!"\n\n" |>.map line
  xs.tail!.foldl (λ x y => x ++ "\n" ++ y) (xs.head!)

#eval println! translate "
ab
dd

gc
"


