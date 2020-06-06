--compile and run
-- ghc cse.hs -o cse
-- ./cse csereport.txt

import System.IO
import System.Environment
import Data.List
import Data.List.Split
import System.Random
import Data.List (intercalate)
import qualified Data.Text as T



main = do
  args <- getArgs
  output <- openFile (head args) ReadMode
  --openFile :: FilePath -> IOMode -> IO Handle.  "Handle" means the file we're reading. In this case, "output"
  --FilePath is just a type synonym for String, simply defined as: type FilePath = String
  --data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

  --hGetContents :: Handle -> IO String
  contents <- hGetContents output

  --lines :: String -> [String] (creates an array of string from the original one, new line characters serving as separators)
  let allLines      = lines contents
  listsCombined <- fillLists allLines [fire,lightning,wind,water,earth]
  print ("Lists are constructed")
  print listsCombined
  putStr $ unlines $map( unwords . map show ) $ listsCombined

  xfunc listsCombined


  hClose output

fillLists :: [String] -> [[Ninja]] -> IO [[Ninja]]
fillLists allLines x@[fire,lightning,wind,water,earth] = do
  let oneLine   = getOneLine allLines
  if oneLine == "" then return x
  else do
  let aninja    = lineToNinja oneLine
  let newx      = addNinjaToList x aninja     --((fire ++ [aninja]):water)
  fillLists (tail allLines) newx

--Ninja array'lerinden oluşan array alır. Hem IO yapar hem Ninja array'lerinden oluşan array döndürür.
xfunc :: [[Ninja]] -> IO [[Ninja]]
xfunc allLists = do
  putStrLn "a) View a Country's Ninja Information"
  putStrLn "b) View All Countries' Ninja Information"
  putStrLn "c) Make a Round Between Ninjas"
  putStrLn "d) Make a Round Between Countries"
  putStrLn "e) Exit"
  userSelection <- getLine
  if userSelection == "e"
  then do
    printJourneyman allLists
    return allLists
  else if userSelection == "a"
  then do
    getCountry allLists
    xfunc allLists
  else if userSelection == "b"
  then do
    printAllCountries allLists
    xfunc allLists
  else if userSelection == "c"
  then do
    allLists <- makeRoundNinjas allLists
    xfunc allLists
  else if userSelection == "d"
  then do
    allLists <- makeRoundCountries allLists
    xfunc allLists
  else do
    xfunc allLists


printJourneyman :: [[Ninja]] -> IO[[Ninja]]
printJourneyman [] = return []
printJourneyman allLists@(x:xs) = do
  let firstNinja@(Ninja a b s d e f g r k) = head x
  if r == 3
  then do
  print (head allLists)
  printJourneyman allLists
  else do
  print("This ninja is not a Journeyman")
  printJourneyman xs



makeRoundNinjas :: [[Ninja]] -> IO [[Ninja]]
makeRoundNinjas allLists = do
  putStrLn "Enter the name of the first ninja: "
  firstName <- getLine
  putStrLn "Enter the country code of the first ninja: "
  firstCountry <- getLine
  putStrLn "Enter the name of the second ninja: "
  secondName <- getLine
  putStrLn "Enter the country code of the second ninja: "
  secondCountry <- getLine
  (allLists, ninja1@(Ninja a b c d e f g h k)) <- findCountry firstCountry firstName allLists
  (allLists, ninja2@(Ninja a2 b2 c2 d2 e2 f2 g2 h2 k2)) <- findCountry secondCountry secondName allLists
  --Both ninjas have been deleted. After the comparison, winner ninja will be added again.


  let abilities1 = calculateAbility f + calculateAbility g
  let abilities2 = calculateAbility f2 + calculateAbility g2
  rand <- drawDouble 0.0 1.0
  putStr "Winner: "
  --Compare scores and add the winner ninja to the list with updated values
  if k > k2 || (k == k2 && abilities1 > abilities2) || (k == k2 && abilities1 == abilities2 && rand > 0.5)
    then do
    let newlists = addNinjaToList allLists (Ninja a b c d e f g (h+1) (k+10))
    print (Ninja a b c d e f g (h+1) (k+10))
    return newlists
    else do--if k < k2 || (k == k2 && abilities2 > abilities1) || (k == k2 && abilities1 == abilities2 && rand <= 2)
    let newlists = addNinjaToList allLists (Ninja a2 b2 c2 d2 e2 f2 g2 (h2+1) (k2+10))
    print (Ninja a2 b2 c2 d2 e2 f2 g2 (h2+1) (k2+10))
    return newlists

makeRoundCountries :: [[Ninja]] -> IO [[Ninja]]
makeRoundCountries allLists = do
  putStrLn "Enter the first country code: "
  c1 <- getLine
  putStrLn "Enter the second country code: "
  c2 <- getLine
  (allLists, ninja1@(Ninja a b c d e f g h k)) <- getFirstNinja c1 allLists
  (allLists, ninja2@(Ninja a2 b2 c2 d2 e2 f2 g2 h2 k2)) <- getFirstNinja c2 allLists
  --Both ninjas have been deleted. After the comparison, winner ninja will be added again.


  let abilities1 = calculateAbility f + calculateAbility g
  let abilities2 = calculateAbility f2 + calculateAbility g2
  rand <- drawDouble 0.0 1.0
  putStr ("Winner: ")


  --Compare scores and add the winner ninja to the list with updated values
  if k > k2 || (k == k2 && abilities1 > abilities2) || (k == k2 && abilities1 == abilities2 && rand > 0.5)
    then do
    let newNinja = checkJourneyman (Ninja a b c d e f g (h+1) (k+10))
    let newlists = addNinjaToList allLists newNinja
    print ( newNinja )
    return newlists
    else do--if k < k2 || (k == k2 && abilities2 > abilities1) || (k == k2 && abilities1 == abilities2 && rand <= 2)
    let newNinja = checkJourneyman (Ninja a2 b2 c2 d2 e2 f2 g2 (h2+1) (k2+10))
    let newlists = addNinjaToList allLists newNinja
    print (newNinja)
    return newlists


checkJourneyman :: Ninja -> Ninja
checkJourneyman nin@(Ninja a b s d e f g r k)
  | r==3 = Ninja a b "Journeyman" d e f g r k
  | r /=3 =nin



--Returns the final allLists(after substracting the first ninja from the given country) and deleted Ninja
getFirstNinja :: String -> [[Ninja]] -> IO ([[Ninja]], Ninja)
getFirstNinja country allLists@[fi,l,wi,wa,ea]
  | country == "F" || country == "f" = do
  return ([tail fi,l,wi,wa,ea], head fi)
  | country == "W" || country == "w" = do
  return ([fi,l,wi,tail wa,ea], head wa)
  | country == "N" || country == "n" = do
  return ([fi,l,tail wi,wa,ea], head wi)
  | country == "E" || country == "e" = do
  return ([fi,l,wi,wa,tail ea], head ea)
  | country == "L" || country == "l" = do
  return ([fi,tail l,wi,wa,ea], head l)


--Returns the final allLists and the Ninja that is deleted
findCountry :: String -> String -> [[Ninja]] -> IO ([[Ninja]], Ninja)
findCountry country name allLists@[fi,l,wi,wa,ea]
  | country == "F" || country == "f" = do
  let x = deleteNinja name fi
  return ([fst x,l,wi,wa,ea], snd x)

  | country == "W" || country == "w" = do
  let x = deleteNinja name wa
  return (([fi,l,wi,fst x,ea], snd x))

  | country == "N" || country == "n" = do
  let x = deleteNinja name wi
  return (([fi,l,fst x,wa,wa], snd x))

  | country == "E" || country == "e" = do
  let x = deleteNinja name ea
  return (([fi,l,wi,wa,fst x], snd x))

  | country == "L" || country == "l" = do
  let x = deleteNinja name l
  return (([fi,fst x,wi,wa,ea], snd x))

--Finds and deletes the ninja in the ninja list and returns the new list and deleted ninja
--First case:  Just get the tail of the list and deleted ninja
--Second case: Append the unmatched ninja to the beginning, and call itself by giving the tail of the ninja list as parameter
--Side Note:   In second case, there are two exact same calls. This is because of getting first and second element of tuple. Can we make it in a one call?
deleteNinja :: String -> [Ninja] -> ([Ninja], Ninja)
deleteNinja name x@(nin@(Ninja a b c d e f g h k):xs)
  | name == a = (xs,nin)
  | name /= a = (nin:(fst $ deleteNinja name xs), (snd $ deleteNinja name xs))



printAllCountries :: [[Ninja]] -> IO [[Ninja]]
printAllCountries allLists
  | length allLists == 0 = return allLists
  | length allLists /= 0 = do
      print (head allLists)
      printAllCountries (tail allLists)

--Each Ninja's countries checked then added country array list
addNinjaToList :: [[Ninja]] -> Ninja -> [[Ninja]]
addNinjaToList allLists@[fi,l,wi,wa,ea] nin@(Ninja a b c d e f g h k) = case b of
  'F' -> [(fi ++ [nin]),l,wi,wa,ea]
  'L' -> [fi,(l ++ [nin]),wi,wa,ea]
  'N' -> [fi,l,(wi ++ [nin]),wa,ea]
  'W' -> [fi,l,wi,(wa ++ [nin]),ea]
  'E' -> [fi,l,wi,wa,(ea ++ [nin])]
  _   -> allLists

lineToNinja :: String -> Ninja
lineToNinja oneLine =
  let
     words     = convertLineToList (oneLine)
     aninja    = createOneNinja(words)
  in (aninja)

--Her bir satırı array'e farklı eleman olarak dizilmiş string arrayinden ilk elemanı (ilk satırı) getirir
getOneLine :: [String] -> String
getOneLine allLines = case allLines of
  []     -> ""
  (x:_)  -> x

--Kelimelerin olduğu arrayi (string arrayi) tek bir ninjaya çevirir
createOneNinja :: [String] -> Ninja
createOneNinja line
  | (line !! 1) == "Wind"  = putScore (Ninja (line !! 0) 'N' "Junior" (read (line !! 2)) (read (line !! 3)) (line !! 4) (line !! 5) 0 0)
  | (line !! 1) /= "Wind"  = putScore (Ninja (line !! 0) ((line !! 1) !! 0) "Junior" (read (line !! 2)) (read (line !! 3)) (line !! 4) (line !! 5) 0 0)


--tek bir stringi(cümleyi) kelimelerden olusan arraye donusturuyor
convertLineToList :: String -> [String]
convertLineToList givenLine =
  splitOn " " (givenLine)



data Ninja = Ninja {name:: String, country:: Char,
status:: String, exam1:: Float,
exam2:: Float, ability1:: String,
ability2:: String, r:: Int,
score:: Float}

instance Show Ninja where
  show (Ninja a b c d e f g h k) = a ++ ", Score: " ++  show k ++ ", Status: " ++ c ++ ", Round: " ++ show h ++ "\n"



fire :: [Ninja] -- add the junior ninjas of Land of Fire to that list
fire = []
lightning :: [Ninja] -- add the junior ninjas of Land of Lightning to that list
lightning = []
water :: [Ninja] -- add the junior ninjas of Land of Water to that list
water = []
wind :: [Ninja] -- add the junior ninjas of Land of Wind to that list
wind = []
earth :: [Ninja] -- add the junior ninjas of Land of Earth to that list
earth = []

-- Ninja's total score is calculated
putScore :: Ninja -> Ninja
putScore nin@(Ninja name county stat exam1 exam2 ab1 ab2 r score) =
  Ninja name county stat exam1 exam2 ab1 ab2 r (0.5*exam1 + 0.3*exam2 + calculateAbility ab1 + calculateAbility ab2 + 10* (fromIntegral r))



calculateAbility :: String -> Float
calculateAbility ability
  | ability == "Clone" = 20
  | ability == "Hit" = 10
  | ability == "Lightning" = 50
  | ability == "Vision" = 30
  | ability == "Sand" = 50
  | ability == "Fire" = 40
  | ability == "Blade" = 20
  | ability == "Summon" = 50
  | ability == "Storm" = 10
  | ability == "Rock" = 20
  | ability == "Water" = 30

drawDouble :: Double -> Double  -> IO Double
drawDouble x y = getStdRandom (randomR (x,y))

getCountry :: [[Ninja]] -> IO [[Ninja]]
getCountry allLists = do
  putStrLn "Enter the country code: "
  country <- getLine
  printCountry allLists country
  return allLists

printCountry :: [[Ninja]] -> String -> IO()
printCountry allLists@[fi,l,wi,wa,ea] country
  | country == "F" || country == "f" = print fi
  | country == "W" || country == "w" = print wa
  | country == "N" || country == "n" = print wi
  | country == "E" || country == "e" = print ea
  | country == "L" || country == "l" = print l

  --  | _              = "no country choosen"
