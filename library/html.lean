

import library.parsec

def elementBegin := char '<' |>.move alphanum |>.skip (char '>')
def elementEnd := string "</" |>.move alphanum |>.skip (char '>')


partial def element _ := 
  let elementAux := alphanum.eitherLazy element
  elementBegin.follow (elementAux)
    |>.follow (elementEnd)
    |>.filter (λ x => x.fst.fst == x.snd)
    |>.map (λ x => x.fst.snd)

#eval element () |>.parse "<p><a>rcx</a></p>"
