def main : IO Unit := 
  IO.getStdin 
    >>= (·.getLine) 
    >>= (·.trim.splitOn " " 
      |>.map String.toInt! 
      |>.foldl Int.add Nat.zero 
      |> IO.println)