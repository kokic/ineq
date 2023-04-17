

-- WIP. 

import library.parsec


inductive MdElement 
  | heading : Nat → String → MdElement
  | line : String → MdElement
  | paragraphline : String → MdElement
  | paragraph : String → MdElement
  | breakline : MdElement
  | bold : String → MdElement
  -- | italic : String → MdElement
  -- | link : String → String → MdElement
  -- | image : String → String → MdElement
  | ulistline : Nat → List MdElement → MdElement
  | ulist : Nat → List MdElement → MdElement
  
  | empty : MdElement
  | indent : MdElement → Nat → MdElement → MdElement
  | markdown : MdElement → MdElement → MdElement


def htmlTag (name html : String) := s!"<{name}>{html}</{name}>"
def li := htmlTag "li"

def relint (n n' : Nat) := if n' - n <= 1 then 0 else 2

partial def MdElement.append : (MdElement -> MdElement -> MdElement) 
  | line a, line b => line s!"{a} {b}"

  | paragraphline a, paragraphline b
  | paragraph a, paragraphline b => paragraph s!"{a} {b}"

  | ulistline n a, ulistline n' b => 
      if n == n' then ulist n [ulistline n a, ulistline n' b]
      else -- let ε := relint n n'
           ulistline n (a ++ [ulistline n' b])
          --  let e := a.getLastD empty |>.append (ulistline n' b)
          --  ulist n' (a.set (a.length - 1) e)

  | ulistline n es, x => ulistline n (es ++ [x])
  
  -- remark: notice ulist must be not empty here
  | ulist m a, ulistline n b => 
      if m == n then ulist m (a ++ [ulistline n b]) 
      else let e := a.getLastD empty |>.append (ulistline n b)
           ulist m (a.set (a.length - 1) e)

/-

  for some reasons, we must assume that the parameter of 
  the ulistline constructor is also a List MdElement, 
  and some modifications need to be made to the parameters of 
  the ulist constructor. specifically, two Nats should 
  be prepared to record the indentation of the list itself 
  and the indentation of the current last part of the element.

-/

      -- let e := a.getLastD empty |>.append (ulistline n b)
      -- ulist (a.set (a.length - 1) e)

      -- if n == 0 then ulist (a ++ [ulistline n b])
      -- else let e := a.getLastD empty |>.append (ulistline n b)
      --      ulist (a.set (a.length - 1) e)

      -- ulist (a ++ [ulist [ulistline n b]])
      -- let e := a.getLastD empty |>.append (ulistline n b)
      -- ulist (a.set (a.length - 1) e)
      
  | ulist n a, paragraphline s => 
      let e := a.getLastD empty |>.append (line s)
      ulist n (a.set (a.length - 1) e)

  -- | markdown x (ulist m l), ulistline n a => 
  --     if m == n then markdown x (ulist m (l ++ [ulistline n a]))
  --     else line "#inner-marked"

  | markdown x y, a => markdown x (y.append a) 
  | a, b => markdown a b

instance : Append MdElement := ⟨MdElement.append⟩ 


def reduceline (xs : List String) := 
  let f := λ x y => x ++ "\n" ++ y
  let body := xs.tail!.foldl f xs.head!
  s!"\n{body}\n"

partial def MdElement.toString : (MdElement -> String) 
  | heading n s => htmlTag s!"h{n}" s
  
  | line s => s

  | paragraphline s
  | paragraph s => htmlTag "p" s

  | ulistline n e => s!"[{n}]-" ++ (e.map (·.toString) |>.reduceString)
  | ulist n xs => htmlTag "ul" (xs.map (·.toString |> li) |> reduceline)
  
  | breakline => "<br>"
  | markdown x y => x.toString ++ "\n" ++ y.toString
  | _ => ""

instance : ToString MdElement := ⟨MdElement.toString⟩ 





def all := satisfy_alt (λ _ => true) |>.plus

def heading := char_alt '#' 
  |>.plus.skip space |>.map (·.length) 
  |>.follow all
  |>.map (λ (n, s) => MdElement.heading n s)

-- def bold := string "**" |>.follow ()

-- def line := all.map MdElement.line |>.trim 

def paragraphLine := all.map MdElement.paragraphline |>.trim 

def ulistLine := spacea.map (·.length)
  |>.skip (string "- ") 
  |>.follow all 
  |>.map (λ (n, s) => MdElement.ulistline n [MdElement.line s])

def element := heading.or ulistLine 
  |>.or paragraphLine



def transmorph : String -> MdElement 
  | s => s |> element.parse
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

#eval println! translate "

# Heading 1
## Heading 2

- item 1
  - subitem 1
  - subitem 2
    - subsubitem 1
    - subsubitem 2
- item 2 and 
same line
- item 3

line 1 and
same line

line 2 and
a
b
c

    Dalvik & Zygote

end
"
