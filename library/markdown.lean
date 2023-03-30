def translate (markdown : String) : String :=
  let lines := markdown.trim.splitOn "\n"
  let htmlLines := lines.map $ fun line =>
    if line.startsWith "#" then
      let level := line.takeWhile (· == '#')
      let content := line.drop level.length
      s!"<h{level.length}>{content.trim}</h{level.length}>"
    else
      s!"<p>{line.trim}</p>"
  htmlLines.foldl (· ++ "\n" ++ ·) ""

#eval println! translate "#Hello \n##Markdown  \n impl in lean."