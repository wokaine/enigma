module Enigma where
  import Data.Char  -- to use functions on characters
  import Data.Maybe -- breakEnigma uses Maybe type
  -- Extra imports added below
  import Data.Tuple -- to use 'swap'
  import Debug.Trace

{- Part 1: Simulation of the Enigma -}

  type Rotor = (String, Int) -- String supplied are the wirings, Num is the knock-on position
  type Reflector = [(Char, Char)] -- List of char pairings
  type Offsets = (Int, Int, Int) -- Triple of integers representing the offsets of each rotor (LR, MR, RR)
  type Stecker = [(Char, Char)] -- List of char pairings
  
  data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets
                | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Stecker

  {-
    First rotor in constructor represents right rotor
    Second rotor represents middle
    Third rotor represents left
    (x, y, z) offsets are the offsets for the left, middle and right rotor
  -}

  {-
    Some variables used for testing, most are taken from Main
  -}
  --enigma1 = (SimpleEnigma rotor3 rotor2 rotor1 reflectorB (0,0,25))
  --plugboard = [('F','T'),('D','U'),('V','A'),('K','W'),('H','Z'),('I','X')]
  --enigma2 = (SteckeredEnigma rotor3 rotor2 rotor1  reflectorB (0,0,25) plugboard)

  -- I found it easier to sanitise the input string first and then pass it through to encode, rather than cleanse each individual character
  encodeMessage :: String -> Enigma -> String
  encodeMessage s e = encodeUncleansedMessage (cleanse s) e 
  
  encodeUncleansedMessage :: String -> Enigma -> String

  -- Encoding for non-steckered enigma
  encodeUncleansedMessage [] (SimpleEnigma rr mr lr ref off) = ""
  encodeUncleansedMessage (' ':xs) (SimpleEnigma rr mr lr ref off) = ' ' : (encodeUncleansedMessage xs (SimpleEnigma rr mr lr ref off))
  encodeUncleansedMessage (x:xs) (SimpleEnigma rr mr lr ref off) = 
    (encryptWithReflection x rr mr lr (keypress lr mr rr off) ref) : (encodeUncleansedMessage xs (SimpleEnigma rr mr lr ref (keypress lr mr rr off)))

  -- Encoding for a steckered enigma
  encodeUncleansedMessage [] (SteckeredEnigma rr mr lr ref off stc) = ""
  encodeUncleansedMessage (' ':xs) (SteckeredEnigma rr mr lr ref off stc) = ' ' : (encodeUncleansedMessage xs (SteckeredEnigma rr mr lr ref off stc))
  encodeUncleansedMessage (x:xs) (SteckeredEnigma rr mr lr ref off stc) = 
    (encryptWithStecker x rr mr lr (keypress lr mr rr off) ref stc) : (encodeUncleansedMessage xs (SteckeredEnigma rr mr lr ref (keypress lr mr rr off) stc))

  cleanse :: String -> String
  cleanse [] = ""
  cleanse (x:xs) | isLower x              = toUpper x : cleanse xs
                 | isUpper x || x == ' '  = x : cleanse xs
                 | otherwise              = "" ++ cleanse xs

  mod26 :: Int -> Int
  mod26 x = x `mod` 26

  reflect :: Char -> Reflector -> Char
  reflect c [] = error "Reflection not found, check reflector is complete"
  reflect c ((a,b):xs) | (toUpper c) == a = b
                       | (toUpper c) == b = a
                       | otherwise        = reflect c xs   

  stecker :: Char -> Stecker -> Char
  stecker c [] = c
  stecker c ((a,b):xs) | (toUpper c) == a = b
                       | (toUpper c) == b = a
                       | otherwise        = stecker c xs

  -- keypress: simulates a single keypress for 3 rotors given their offset positions
  keypress :: Rotor -> Rotor -> Rotor -> Offsets -> Offsets
  keypress (_, kposlr) (_, kposmr) (_, kposrr) (x,y,z) | (mod26(z+1)) == (kposrr+1) && (mod26(y+1)) == (kposmr+1) = (mod26(x+1), mod26(y+1), mod26(z+1))
                                                       | (mod26(z+1)) == (kposrr+1)                               = (mod26(x), mod26(y+1), mod26(z+1))
                                                       | otherwise                                                = (mod26(x), mod26(y), mod26(z+1))

  rotorAlphaZip :: Rotor -> [(Char, Char)]
  rotorAlphaZip (wirings, _)  = [(x, y) | (x, y) <- zip "ABCDEFGHIJKLMNOPQRSTUVWXYZ" wirings]

  searchWirings :: Char -> [(Char, Char)] -> Char
  searchWirings c [] = error "Empty array"
  searchWirings c ((a,b):xs) | (toUpper c) == a = b
                             | otherwise        = searchWirings c xs

  -- pos2char: given its index in the alphabet, return the character at that index. opposite of alphapos
  pos2char :: Int -> Char
  pos2char n = chr (ord 'A' + n)

  -- offsetChar: will offset a character by n positions
  offsetChar :: Char -> Int -> Char
  offsetChar c n = pos2char(mod26 ((alphaPos c) + n))

  {-
    The following functions pass in the above functions, they are there to make other encryption functions more readable,
    rather than having the whole process jammed into one function.
  -}

  -- rotoriseForward: encrypts a character with a rotor going forward (pre-reflection), accounts for offset
  rotoriseForward :: Char -> Rotor -> Int -> Char
  rotoriseForward c r offset = offsetChar (searchWirings (offsetChar c offset) (rotorAlphaZip r)) (-offset)

  rotoriseBackward :: Char -> Rotor -> Int -> Char
  rotoriseBackward c r offset = offsetChar (searchWirings (offsetChar c offset) (map swap (rotorAlphaZip r))) (-offset)

  -- preReflection: Puts a character through three rotors before reflection
  preReflection :: Char -> Rotor -> Rotor -> Rotor -> Offsets -> Char
  preReflection c rr mr lr (x, y, z) = (rotoriseForward (rotoriseForward (rotoriseForward c rr z) mr y) lr x)

  postReflection :: Char -> Rotor -> Rotor -> Rotor -> Offsets -> Char
  postReflection c rr mr lr (x, y, z) = (rotoriseBackward (rotoriseBackward (rotoriseBackward c lr x) mr y) rr z)

  -- encryptWithReflection: Fully encrypts a character w/o steckering
  encryptWithReflection :: Char -> Rotor -> Rotor -> Rotor -> Offsets -> Reflector -> Char
  encryptWithReflection c rr mr lr off ref = (postReflection (reflect (preReflection c rr mr lr off) ref) rr mr lr off)

  encryptWithStecker :: Char -> Rotor -> Rotor -> Rotor -> Offsets -> Reflector -> Stecker -> Char
  encryptWithStecker c rr mr lr off ref stc = stecker (encryptWithReflection (stecker c stc) rr mr lr off ref) stc


{- Part 2: Finding the Longest Menu -}

  type Menu = [Int] 
  type Crib = [(Char, Char)]
  type FullCrib = [((Char, Char), Int)] -- I decided to make my own type that zips a char pairing to its index so its easier to then take each index for a menu

  longestMenu :: Crib -> Menu
  longestMenu crib = takeIndices(selectLongest(createAllMenus (createCrib crib) (createCrib crib)))

  {-
    More variables used for testing. These were based off the example in the brief.
  -}
  -- cribText1 = "WETTERVORHERSAGEBISKAYA"
  -- message1 = "RWIVTYRESXBFOGKUHQBAISE"
  -- crib1 = createCrib (zip cribText1 message1)
  -- cribpiece1 = (('E', 'W'), 1::Int)

  createCrib :: Crib -> FullCrib
  createCrib crib = [((x, y), z) | ((x, y), z) <- zip crib [0..length crib]]

  findInPlaintext :: ((Char, Char), Int) -> FullCrib -> ((Char, Char), Int)
  findInPlaintext ((p1, c1), i) [] = ((p1, c1), i)
  findInPlaintext ((p1, c1), i) (((p2, c2), j):xs) | (toUpper c1) == (toUpper p2) && i /= j = ((p2, c2), j)
                                                   | otherwise                              = findInPlaintext ((p1, c1), i) xs
  
  removeCribPiece :: ((Char, Char), Int) -> FullCrib -> FullCrib
  removeCribPiece _ [] = []
  removeCribPiece x (y:ys) | x == y    = removeCribPiece x ys
                           | otherwise = y : removeCribPiece x ys

  -- returns all occurences of a piece in the plaintext
  findAllOccurrences :: ((Char, Char), Int) -> FullCrib -> FullCrib
  findAllOccurrences x ys | findInPlaintext x ys == x = []
                          | otherwise = [findInPlaintext x ys] ++ findAllOccurrences x (removeCribPiece (findInPlaintext x ys) ys)

  -- Create a menu using first occurence of a piece, if visualised in a tree this will be equivalent of going left each time
  createLeftmostMenu :: ((Char, Char), Int) -> FullCrib -> FullCrib
  createLeftmostMenu piece crib | findInPlaintext piece crib == piece = [piece]
                                | otherwise                           = [piece] ++ createLeftmostMenu nextpiece newcrib
                          where nextpiece = findInPlaintext piece crib
                                newcrib = removeCribPiece piece crib

  -- For a node with multiple options, generate each nodes leftmost tree
  allLeftMenus :: FullCrib -> FullCrib -> [FullCrib]
  allLeftMenus [] crib = []
  allLeftMenus (x:xs) crib = [createLeftmostMenu x crib] ++ allLeftMenus xs newcrib
                          where newcrib = removeCribPiece x crib                              

  selectLongest :: [FullCrib] -> FullCrib
  selectLongest [] = []
  selectLongest (x:xs) = foldl (\z y -> if length(y) > length(z) then y else z) x xs

  -- Selects the node from the longest left menu
  selectBestNode :: [FullCrib] -> ((Char, Char), Int)
  selectBestNode menu = head (selectLongest menu) 

  createLongest :: ((Char, Char), Int) -> FullCrib -> FullCrib
  createLongest piece crib | findInPlaintext piece crib == piece = [piece]
                           | length(f) > 1                       = [piece] ++ createLongest bestNode newcrib
                           | otherwise                           = [piece] ++ createLongest nextpiece newcrib
                    where f = findAllOccurrences piece crib
                          newcrib = removeCribPiece piece crib
                          bestNode = selectBestNode (allLeftMenus f newcrib)
                          nextpiece = findInPlaintext piece crib

  -- We need two cribs to go through, one to recurse and get all starting points, the other to pass in to for our menu search that must remain constant
  createAllMenus :: FullCrib -> FullCrib -> [FullCrib]
  createAllMenus [] cribconstant = []
  createAllMenus (x:xs) cribconstant = [createLongest x cribconstant] ++ createAllMenus xs cribconstant

  takeIndices :: FullCrib -> [Int]
  takeIndices [] = []
  takeIndices (((_,_),i):xs) = [i] ++ takeIndices xs
  

{- Part 3: Simulating the Bombe -}
  
  breakEnigma :: Crib -> Maybe (Offsets, Stecker)
  breakEnigma crib 
   = tryAllOffsetConfigs createAllConfigs (createAllInitialSteckers (head (longestFullCrib crib)) 0 []) [] (longestFullCrib crib) rotor1 rotor2 rotor3 reflectorB

  --Assume R1 R2 R3 (L M R) and reflectorB
  --Initial offsets (0,0,0)

  {-
    This is the menu used for testing, it is based off of the one in the brief
  -}
  -- menu = [(('E','W'),1::Int), (('W','R'),0::Int), (('R', 'Y'), 5::Int), (('Y','S'),21::Int), (('S','O'),12::Int), 
  --   (('O','E'),7::Int), (('E', 'T'),4::Int), (('T','V'), 3::Int), (('V', 'R'),6::Int), (('R','S'),8::Int), (('S', 'B'),18::Int), (('B','H'),16::Int), (('H','X'),9::Int)]

  -- Need indices of crib
  longestFullCrib :: Crib -> FullCrib
  longestFullCrib crib = selectLongest(createAllMenus (createCrib crib) (createCrib crib))

  createSteckerPiece :: Char -> ((Char, Char), Int)  -> Rotor -> Rotor -> Rotor -> Offsets -> Reflector -> (Char, Char)
  createSteckerPiece c ((p1, c1),i) lr mr rr off ref = ((encryptWithReflection c rr mr lr off ref), c1) 

  isContradiction :: (Char, Char) -> (Char, Char) -> Bool
  isContradiction (a, b) (c, d) | (a, b) == (c, d) = False
                                | (a, b) == (d, c) = False 
                                | a == b || c == d = False
                                | a == c || a == d = True 
                                | b == c || b == d = True
                                | otherwise        = False
  
  checkContradiction :: (Char, Char) -> Stecker -> Bool
  checkContradiction _ [] = False
  checkContradiction pair (x:xs) | isContradiction pair x == True = True
                                 | otherwise                      = checkContradiction pair xs

  changeOffsetByCrib :: Offsets -> Rotor -> Rotor -> Rotor -> ((Char, Char), Int) -> Offsets
  changeOffsetByCrib offsets lr mr rr ((p1, c1), i) = last(take (i+2) (iterate (keypress lr mr rr) offsets))
  
  -- try generate stecker for one set of offsets and one stecker piece
  tryBreak :: Offsets -> (Char, Char) -> Stecker -> FullCrib -> Rotor -> Rotor -> Rotor -> Reflector -> Maybe (Offsets, Stecker)
  tryBreak offsets _ stecker [] _ _ _ _ = Just (offsets, stecker)
  tryBreak offsets (a, b) stecker (x:xs) lr mr rr ref | checkContradiction (a, b) stecker == True = Nothing
                                                      | otherwise = tryBreak offsets newpiece (stecker ++ [(a, b)]) xs lr mr rr ref
                                                where newoffsets = changeOffsetByCrib offsets lr mr rr x
                                                      newpiece = createSteckerPiece a x lr mr rr newoffsets ref

  -- This function is just incase you want everything outputted rather than "Nothing"
  tryBreak' :: Offsets -> (Char, Char) -> Stecker -> FullCrib -> Rotor -> Rotor -> Rotor -> Reflector -> (Offsets, Stecker)
  tryBreak' offsets _ stecker [] _ _ _ _ = (offsets, stecker)
  tryBreak' offsets (a, b) stecker (x:xs) lr mr rr ref | checkContradiction (a, b) stecker == True = (offsets, stecker ++ [(a,b)])
                                                       | otherwise = tryBreak' offsets newpiece (stecker ++ [(a, b)]) xs lr mr rr ref
                                                 where newoffsets = changeOffsetByCrib offsets lr mr rr x
                                                       newpiece = createSteckerPiece a x lr mr rr newoffsets ref

  -- Create all initial stecker pairs to run through
  createAllInitialSteckers :: ((Char, Char), Int) -> Int -> [(Char, Char)] -> [(Char, Char)]
  createAllInitialSteckers _ 26 configs = configs
  createAllInitialSteckers ((p1, c1), i) n configs = createAllInitialSteckers ((p1, c1), i) (n+1) (configs ++ [(p1, (offsetChar p1 n))])  

  tryAllInitialSteckers :: Offsets -> [(Char, Char)] -> Stecker -> FullCrib -> Rotor -> Rotor -> Rotor -> Reflector -> Maybe (Offsets, Stecker)
  tryAllInitialSteckers _ [] _ _ _ _ _ _ = Nothing
  tryAllInitialSteckers offsets (x:xs) stecker menu lr mr rr ref | tryBreak offsets x [] menu lr mr rr ref == Nothing = tryAllInitialSteckers offsets xs [] menu lr mr rr ref
                                                                 | otherwise = tryBreak offsets x [] menu lr mr rr ref

  -- For output:
  -- tryBreak offsets x [] menu lr mr rr ref == Nothing = trace (show (tryBreak' offsets x [] menu lr mr rr ref)) (tryAllInitialSteckers offsets xs [] menu lr mr rr ref)
  -- otherwise = trace (show x) (tryBreak offsets x [] menu lr mr rr ref)

  createAllConfigs :: [Offsets]
  createAllConfigs = [(x,y,z) | x <- [0..25], y <- [0..25], z <- [0..25]]

  tryAllOffsetConfigs :: [Offsets] -> [(Char, Char)] -> Stecker -> FullCrib -> Rotor -> Rotor -> Rotor -> Reflector -> Maybe (Offsets, Stecker)
  tryAllOffsetConfigs [] _ _ _ _ _ _ _ = Nothing
  tryAllOffsetConfigs (x:xs) allinitialpairs stecker menu lr mr rr ref 
    | tryAllInitialSteckers x allinitialpairs [] menu lr mr rr ref == Nothing = tryAllOffsetConfigs xs allinitialpairs stecker menu lr mr rr ref
    | otherwise = (tryAllInitialSteckers x allinitialpairs [] menu lr mr rr ref)

  -- our final break enigma function takes a lot of parameters, but a lot can be generated just from one crib:
  -- [Offsets] - createAllConfigs
  -- [(Char, Char)] - (createAllInitialSteckers (head (longestFullCrib crib)) 0 [])
  -- Stecker - just initialise as empty list []
  -- FullCrib - (longestFullCrib crib)
  -- Rotor - rotor1
  -- Rotor - rotor2
  -- Rotor - rotor3
  -- Reflector - reflectorB
  
{- Useful definitions and functions -}

   -- substitution cyphers for the Enigma rotors
   -- as pairs of (wirings, knock-on position)
   -- knock-on position is where it will cause the next left wheel to
   -- advance when it moves past this position
 
        --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  rotor1=("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int)
  rotor2=("AJDKSIRUXBLHWTMCQGZNPYFVOE",5::Int)
  rotor3=("BDFHJLCPRTXVZNYEIWGAKMUSQO",22::Int)
  rotor4=("ESOVPZJAYQUIRHXLNFTGKDCMWB",10::Int)
  rotor5=("VZBRGITYUPSDNHLXAWMJQOFECK",0::Int)

  {- the standard Enigma reflector (Reflector B)
    swapped A<->Y, B<->R, C<->U,D<->H, E<->Q, F<->S, G<->L, 
            I<->P, J<->X, K<->N, M<->O, T<->Z,V<->W
  -}
  reflectorB= [('A','Y'),
              ('B','R'),
              ('C','U'),
              ('D','H'),
              ('E','Q'),
              ('F','S'),
              ('G','L'),
              ('I','P'),
              ('J','X'),
              ('K','N'),
              ('M','O'),
              ('T','Z'),
              ('V','W')]

  {- alphaPos: given an uppercase letter, returns its index in the alphabet
     ('A' = position 0; 'Z' = position 25)
   -}
  alphaPos :: Char -> Int
  alphaPos c = (ord c) - ord 'A'
