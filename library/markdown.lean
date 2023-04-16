

-- WIP. 

import library.parsec


inductive MdElement 
  | heading : Nat → String → MdElement
  | paragraphline : String → MdElement
  | paragraph : String → MdElement
  | breakline : MdElement


  -- | bold : String → MdElement
  -- | italic : String → MdElement
  -- | link : String → String → MdElement
  -- | image : String → String → MdElement
  | empty : MdElement
  | markdown : MdElement → MdElement → MdElement


def MdElement.append : (MdElement -> MdElement -> MdElement) 
  | paragraphline a, paragraphline b 
  | paragraph a, paragraphline b => paragraph (s!"{a} {b}")
  | markdown x y, a => markdown x (y.append a) 
  | a, b => markdown a b

instance : Append MdElement := ⟨MdElement.append⟩ 


def MdElement.toString : (MdElement -> String) 
  | heading n s => s!"<h{n}>{s}</h{n}>"
  | paragraphline s 
  | paragraph s => s!"<p>{s}</p>"
  | breakline => "<br>"
  | markdown x y => x.toString ++ y.toString
  | _ => ""

instance : ToString MdElement := ⟨MdElement.toString⟩ 

def all := satisfy_alt (λ _ => true) |>.plus

def heading := char_alt '#' |>.plus.map (·.length) 
  |>.follow all
  |>.map (λ (n, s) => MdElement.heading n s)

def paragraphLine := all.map MdElement.paragraphline 

def line := heading.or paragraphLine


def transmorph : String -> MdElement 
  | s => s.trim |> line.parse 
                |>.getD (MdElement.empty, "") 
                |>.fst

partial def translate (s : String) := 
  let xs := s.trim.splitOn "\n" |>.map transmorph
  xs.tail!.foldl MdElement.append (xs.headD MdElement.empty) |>.toString

  -- let size := source.length
  -- let rec translate_aux (pos : Nat) (xs : List String) := 
    -- let read n := source[pos + n]!.trim
    -- let (curr, peek) := (read 0, read 1)
    -- let when (p : Bool) (r : List String) := if p then r else xs
    -- if pos >= size then xs else
    -- match curr with
      -- | "" => translate_aux (pos + 1) (xs ++ ["<br>"])
      -- | s => translate_aux (pos + 1) (xs ++ [s])  
  -- translate_aux 0 []



def read_file (path : String) : IO String := 
  IO.FS.Handle.mk path IO.FS.Mode.read >>=
    λ file ↦ 
      bind (file.readToEnd) λ contents ↦
      pure contents 


-- #eval read_file "./library/markdown-in.md"
--   >>= (λ s => s |> translate |> pure)
--   >>= IO.println

#eval translate "

# Heading 1
## Heading 2

line 1 and
same line

line 2 and
a
b
c

Dalvik & Zygote

end
"
