
structure HashIds :=
  (salt : String)
  (alphabet : String)
  (separators : String)
  -- (minHashLength : Nat)
  -- (guards : String)

#check HashIds -- just checking if the structure is defined correctly

-- javascript Number.MAX_SAFE_INTEGER + 1
def MAX_INTEGER : UInt64 := 9007199254740992


def defaultSalt := ""
def defaultAlphabet := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
def defaultSeparators := "cfhistuCFHISTU"

def defaultHashIds : HashIds := HashIds.mk defaultSalt defaultAlphabet defaultSeparators


def salt := defaultSalt
def alphabet := defaultAlphabet
def separators := defaultSeparators



def initialEncode (numbers: List UInt64)
                  (separators: List Char)
                  (bufferSeed: String)
                  (currentIndex: Int)
                  (alphabet: String)
                  (currentReturnString: String) : String × String 
  := sorry



def ensureMinimalLength (halfLength: Int) 
                        (alphabet: String) 
                        (returnString: String): String
  := sorry



def addGuardsIfNecessary (encodedString: String) 
                         (numbersHash: UInt32): String
  := sorry



def encode : List UInt64 -> Except String String
  | [] => Except.ok ""
  | xs => if xs.contains MAX_INTEGER 
      then Except.error s!"number can not be greater than {MAX_INTEGER}"
      else let len := alphabet.length
           let numbersHash := List.range (xs.length)
             |>.map (λ i => (xs[i]! % (i + 100)).toUInt32 )
             |>.foldr UInt32.add 0
          let initial := alphabet.toList[(numbersHash % len).toNat]!
          let initial' := initial.toString
          let (encodedString, encodingAlphabet) := initialEncode 
            xs separators.toList initial' 0 alphabet initial'
          let returnString := addGuardsIfNecessary encodedString numbersHash
          Except.ok (ensureMinimalLength (len / 2) encodingAlphabet returnString)


-- #eval encode []

