


structure HashIds :=
  (salt : String)
  (alphabet : String)
  (separators : String)
  -- (minHashLength : Nat)
  -- (guards : String)

#check HashIds -- just checking if the structure is defined correctly




structure AlphabetAndSeparators := 
  (alphabet: String) 
  (separators: String)
  (guards: String := "")

structure ShuffleData := 
  (alphabet: List Char)
  (salt: String)
  (cumulative: Nat) 
  (saltReminder: Nat)

structure HashData := 
  (hash: String) 
  (current: UInt64)





-- javascript Number.MAX_SAFE_INTEGER + 1
def MAX_INTEGER : UInt64 := 9007199254740992


def defaultSalt := ""
def defaultAlphabet := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
def defaultSeparators := "cfhistuCFHISTU"
def defaultMinimalHashLength := 0

def defaultHashIds : HashIds := HashIds.mk defaultSalt defaultAlphabet defaultSeparators


def THIS_IS_MY_SALT := "this is my salt"





def calculateAlphabetAndSeparators (userAlphabet : String) : AlphabetAndSeparators 
  := sorry


def salt := defaultSalt
def alphabet := defaultAlphabet
def separators := defaultSeparators
def hashLength := defaultMinimalHashLength

def guards := "xxxxx" -- WARNING!






partial def shuffle (data: ShuffleData) (currentPosition: Nat) (limit: Nat) : ShuffleData :=
  if currentPosition < limit then data
  else let currentAlphabet := data.alphabet
       let saltReminder := data.saltReminder % data.salt.length
       let asciiValue := (data.salt.get! (String.Pos.mk saltReminder)).toNat
       let cumulativeValue := data.cumulative + asciiValue
       let positionToSwap := (asciiValue + saltReminder + cumulativeValue) % currentPosition
       let currentAlphabet' := currentAlphabet.set positionToSwap (currentAlphabet[currentPosition]!)
       let currentAlphabet'' := currentAlphabet'.set currentPosition (currentAlphabet[positionToSwap]!)
       shuffle (ShuffleData.mk currentAlphabet'' data.salt cumulativeValue (saltReminder + 1)) 
               (currentPosition - 1) limit



def consistentShuffle (alphabet: String) : (String -> String)
  | "" => alphabet
  | salt => let initial := ShuffleData.mk alphabet.toList salt 0 0
            (shuffle initial (alphabet.length - 1) 1).alphabet.asString





partial def hashAux (number: UInt64) (alphabet: List Char) (data: HashData): HashData := 
  if data.current <= 0 then data 
  else let hashCharacter := alphabet[(data.current % alphabet.length).toNat]!
       let current := data.current / alphabet.length.toUInt64
       hashAux number alphabet (HashData.mk s!"{hashCharacter}{data.hash}" current)

def hash (input: UInt64) (alphabet: String): String := 
  (hashAux input (alphabet.toList) (HashData.mk "" input)).hash






partial def initialEncode (numbers: List UInt64)
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
       let returnString := if currentIndex + 1 >= numbers.length 
         then currentReturnString ++ last
         else let nextNumber := currentNumber.toNat % ((last.get! 0).toNat + currentIndex)
              let sepsIndex := nextNumber % separators.length
              currentReturnString ++ last ++ separators[sepsIndex]!.toString
       initialEncode numbers separators bufferSeed (currentIndex + 1) alphabet' returnString
-- decreasing_by sorry




partial def ensureMinimalLength (halfLength: Nat) 
                        (alphabet: String) 
                        (returnString: String): String := 
  if returnString.length >= hashLength then returnString
  else let alphabet' := consistentShuffle alphabet alphabet
       let returnString' := alphabet'.drop halfLength 
                         ++ returnString 
                         ++ alphabet'.extract 0 (String.Pos.mk halfLength)
       ensureMinimalLength halfLength alphabet' returnString'
-- decreasing_by sorry





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






-- require RegExp
def extractLotteryCharAndHashs (initialSplit : List String) : Char × List String :=
  let separatorsRegex : String := s!"[{separators}]"
  let i : Nat := if initialSplit.length = 2 ∨ initialSplit.length = 3 then 1 else 0
  let ithElementOfSplit : String := initialSplit.get! i
  let lotteryChar : Char := ithElementOfSplit.get! 0
  let breakdown : List String :=
    let substr : String := ithElementOfSplit.extract (String.Pos.mk 1) 
      (String.Pos.mk ithElementOfSplit.length)
    substr.replace separatorsRegex " " |>.splitOn " "
  (lotteryChar, breakdown)



/-


-/



partial def unhashAux (input: List Char) 
              (alphabet: String) 
              (alphabetLengthU64: UInt64) 
              (currentNumber: UInt64) 
              (currentIndex: Nat): UInt64 :=
  if currentIndex >= input.length then currentNumber
  else let position := alphabet.find (· == input[currentIndex]!) |>.byteIdx
       let exp := input.length - currentIndex - 1
       let factor := alphabetLengthU64.toNat ^ exp |>.toUInt64
       let number := currentNumber + (position.toUInt64 * factor)
       unhashAux input alphabet alphabetLengthU64 number (currentIndex + 1)


def unhash (input: String) (alphabet: String): UInt64 :=
  unhashAux input.toList alphabet alphabet.length.toUInt64 0 0

def unhashSubHashes (hashes: List String) 
                    (lottery: Char) 
                    (currentReturn: List UInt64) 
                    (alphabet: String): List UInt64 :=

  let rec unhashSubHashesAux (hashes: List String) 
                             (currentReturn: List UInt64) 
                             (alphabet: String): List UInt64 :=
    match hashes with
      | [] => currentReturn
      | h :: t =>
        let buffer := s!"{lottery}{salt}{alphabet}"
        let alphabet' := consistentShuffle alphabet 
          (buffer.extract 0 (String.Pos.mk alphabet.length))
        let decoded := unhash h alphabet'
        unhashSubHashesAux t (currentReturn ++ [decoded]) alphabet'
      
  unhashSubHashesAux hashes currentReturn alphabet







-- require RegExp
def decode (hash : String) : Except String (List UInt64) :=
  if hash.isEmpty then Except.ok []
  else let guardsRegex := s!"[{guards}]"
       let hashWithSpacesInsteadOfGuards := hash.replace guardsRegex " "
       let initialSplit := hashWithSpacesInsteadOfGuards.splitOn " "
       let (lottery, hashBreakdown) := extractLotteryCharAndHashs initialSplit   
       let returnValue := unhashSubHashes hashBreakdown lottery [] alphabet
       match encode returnValue with
         | Except.ok s => Except.ok (if s == hash then [] else returnValue)
         | _ => Except.error "decode error"





