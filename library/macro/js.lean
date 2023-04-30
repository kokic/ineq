
#check Lean.Parser.Category

declare_syntax_cat JSExpr
syntax term "?" term ":" term : term
syntax "js: (" term "," term ")" : term

macro_rules
  | `($t:term ? $u:term : $v:term) => `(if $t then $u else $v)
  | `(js: ($_, $u:term)) => `($u)


#eval false ? "YES₁" : true ? "YES₂" : "NO₂"
#eval (1 + 1 >= 2) ? "right" : "idk"





