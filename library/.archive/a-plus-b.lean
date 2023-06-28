
#check (IO.getStdin >>= (·.getLine) : IO String)
-- #check (IO.getStdin >>= λ x => x.getLine : IO String)
-- #check (IO.getStdin >>= fun x => x.getLine : IO String)
-- #check (IO.getStdin >>= IO.FS.Stream.getLine : IO String)

def main : IO Unit := 
  IO.getStdin 
    >>= (·.getLine) 
    >>= (·.trim.splitOn " " 
      |>.map String.toInt! 
      |>.foldl Int.add Nat.zero 
      |> IO.println)