
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
def defaultMinimalHashLength := 0

def defaultHashIds : HashIds := HashIds.mk defaultSalt defaultAlphabet defaultSeparators


def salt := defaultSalt
def alphabet := defaultAlphabet
def separators := defaultSeparators
def hashLength := defaultMinimalHashLength

def guards := "xxxxx" -- WARNING!

structure ShuffleData := 
  (alphabet: List Char)
  (salt: String)
  (cumulative: Int) 
  (saltReminder: Int)


def consistentShuffle (alphabet: String) : (String -> String)
  | "" => alphabet
  | salt => let initial := ShuffleData.mk alphabet.toList salt 0 0
            "" -- shuffle (initial, alphabet.length - 1, 1).alphabet.joinToString(emptyString)


def hash (input: UInt64) (alphabet: String): String := sorry

def initialEncode (numbers: List UInt64)
                  (separators: List Char)
                  (bufferSeed: String)
                  (currentIndex: Nat)
                  (alphabet: String)
                  (currentReturnString: String) : String × String := 
  if currentIndex >= numbers.length then (currentReturnString, alphabet)
  else let currentNumber := numbers[currentIndex]!
       let buffer := bufferSeed ++ salt ++ alphabet
       let alphabet' := consistentShuffle alphabet (buffer.extract 0 (String.Pos.mk alphabet.length))
       let last := hash currentNumber alphabet'
       ("", "") 



def ensureMinimalLength (halfLength: Nat) 
                        (alphabet: String) 
                        (returnString: String): String := 
  if returnString.length >= hashLength then returnString
  else let alphabet' := consistentShuffle alphabet alphabet
       let returnString' := alphabet'.drop halfLength 
                         ++ returnString 
                         ++ alphabet'.extract 0 (String.Pos.mk halfLength)
       ensureMinimalLength halfLength alphabet' returnString'
decreasing_by sorry


def guardIndex (numbersHash: UInt32) (returnString: String) (index: Nat): Nat := 
  (numbersHash.toNat + returnString.toList[index]!.toNat) % guards.length


def addGuardsIfNecessary (encodedString: String) 
                         (numbersHash: UInt32): String := 
  if encodedString.length >= hashLength then encodedString
  else let guard := guards.toList[guardIndex numbersHash encodedString 0]!
       let returnString := encodedString ++ guard.toString
       if returnString.length >= hashLength then returnString
       else let guard' := guards.toList[guardIndex numbersHash returnString 2]!
            returnString ++ guard'.toString




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
