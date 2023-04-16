

-- WIP. 

import library.parsec


def heading := char_alt '#' |>.plus.map (·.length)
  |>.follow (satisfy_alt (· != '\n') |>.plus)
  |>.map (λ (n, s) => s!"<h{n}>{s}</h{n}>")



def paragraphLine := satisfy_alt (λ _ => true) |>.plus

-- def newline := string "\n\n"

-- def line (s : String) := 
--   let s := s.trim
--   let p := heading |>.or paragraph
--   match p.parse s with
--     | some x => x.fst
--     | none => s


-- Dalvik & Zygote

inductive MdElement
  | heading : Nat → String → MdElement
  | paraline : String → MdElement
  -- | bold : String → MdElement
  -- | italic : String → MdElement
  -- | link : String → String → MdElement
  -- | image : String → String → MdElement
  
-- def append : MdElement -> MdElement -> MdElement := match (a, b) with
--   | 
--   | 

def MdElement.append : (MdElement -> MdElement -> MdElement) 
  | a, b => a


instance : Append MdElement := ⟨MdElement.append⟩ 

partial def translate (s : String) := 
  let source := s.trim.splitOn "\n" 
  let size := source.length

  let rec translate_aux (pos : Nat) (xs : List String) := 
    let read n := source[pos + n]!.trim
    let (curr, peek) := (read 0, read 1)
    let when (p : Bool) (r : List String) := if p then r else xs
    if pos >= size then xs else
    match curr with
      | "" => translate_aux (pos + 1) (xs ++ ["<br>"])
      | s => translate_aux (pos + 1) (xs ++ [s])  
  translate_aux 0 []



def read_file (path : String) : IO String := 
  IO.FS.Handle.mk path IO.FS.Mode.read >>=
    λ file ↦ 
      bind (file.readToEnd) λ contents ↦
      pure contents 


#eval read_file "./library/markdown-in.md"
  >>= (λ s => s |> translate |> pure)
  >>= IO.println
