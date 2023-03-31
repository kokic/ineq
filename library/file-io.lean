
def write_file path s :=
  IO.FS.Handle.mk path IO.FS.Mode.write >>= 
    λ file ↦ 
      bind (file.putStr s) λ _ ↦
      bind (file.flush) λ _ ↦
      pure ()

def write_file_do path (s : String) := do
  let file ← IO.FS.Handle.mk path IO.FS.Mode.write
  file.putStr s
  file.flush

def read_file (path : String) : IO String := 
  IO.FS.Handle.mk path IO.FS.Mode.read >>=
    λ file ↦ 
      bind (file.readToEnd) λ contents ↦
      pure contents

def read_file_do (path : String) : IO String := do
  let handle ← IO.FS.Handle.mk path IO.FS.Mode.read
  let contents ← handle.readToEnd
  pure contents


-- def main := 

-- write_file "./a.txt" "well-too"

#eval read_file "./IUTT.lean" >>= IO.println


