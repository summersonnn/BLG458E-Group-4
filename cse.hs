--compile and run
-- ghc cse.hs -o cse
-- ./cse csereport.txt


import System.IO
import System.Environment
import Data.List
import Data.List.Split
import System.Random
import Data.List (intercalate)
import Data.List (sortBy)
import Data.Ord (comparing)
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
  printAllCountries listsCombined

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
    countryJourneymans allLists
    return allLists
  else if userSelection == "a"
  then do
    newLists <- getCountry allLists
    xfunc newLists
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
    
    
    
    
-------------------------------------------------------Functions in Part A----------------------------------------------------------------------
getCountry :: [[Ninja]] -> IO [[Ninja]]
getCountry allLists = do
  putStrLn "Enter the country code: "
  countryCode <- getLine
  printCountry allLists countryCode
  return allLists
  
  
printCountry :: [[Ninja]] -> String -> IO ()
printCountry allLists@[fi,l,wi,wa,ea] country
  | country == "F" || country == "f" = do
    putStrLn $ showZ fi
  | country == "W" || country == "w" = do
    putStrLn $ showZ wa
  | country == "N" || country == "n" = do
    putStrLn $ showZ wi
  | country == "E" || country == "e" = do
    putStrLn $ showZ ea
  | country == "L" || country == "l" = do
    putStrLn $ showZ l
  | otherwise = do
    print "Invalid country code"
    
    
  
sortNinjas :: [Ninja] -> [Ninja]
sortNinjas countryList = sortBy sortby_score_and_round countryList



sortby_score_and_round nin1@(Ninja a b s d e f g round1 score1) nin2@(Ninja a2 b2 s2 d2 e2 f2 g2 round2 score2)
  | round1 > round2 = GT
  | round1 < round2 = LT
  | round1 == round2 = compare score2 score1
  
  
  

--------------------------------------------------------Functions in Part B--------------------------------------------------------------------
printAllCountries :: [[Ninja]] -> IO [[Ninja]]
printAllCountries allLists
  | length allLists == 0 = do
      putStrLn ""
      return allLists
  | length allLists /= 0 = do
      putStr $ showZ (head allLists)
      printAllCountries (tail allLists)
      
      

showZ :: Show a => [a] -> String
showZ = intercalate "" . map show



---------------------------------------------------------Functions in Part C--------------------------------------------------------------------
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
  (newLists, ninja1@(Ninja a b c d e f g h k)) <- findCountry firstCountry firstName allLists
  (newLists, ninja2@(Ninja a2 b2 c2 d2 e2 f2 g2 h2 k2)) <- findCountry secondCountry secondName newLists
  --Both ninjas have been deleted. After the comparison, winner ninja will be added again.

  if (a == "No Ninja" || a2 == "No Ninja")
    then do putStrLn "No such Ninja. Please make sure you type the name of the Ninja correctly.\n"
            return allLists
    else if (a == "No Country" || a2 == "No Country")
    then do putStrLn "No such Country. Please make sure you type the name of the country correctly.\n"
            return allLists
    else do
      let array1 = giveMeCorrectList firstCountry allLists
      let array2 = giveMeCorrectList secondCountry allLists
    
      (_,canFight1) <- printJourneyman array1 False
      (_,canFight2) <- printJourneyman array2 False
      if (canFight1 == True)
        then do putStrLn (charToCountry b ++ " country cannot be included in a fight\n")
                return allLists
      else if (canFight2 == True) 
        then do putStrLn (charToCountry b2 ++ " country cannot be included in a fight\n")
                return allLists
      else do
        let abilities1 = calculateAbility f + calculateAbility g
        let abilities2 = calculateAbility f2 + calculateAbility g2
        rand <- drawDouble 0.0 1.0
        putStr "Winner: "
        --Compare scores and add the winner ninja to the list with updated values
        if k > k2 || (k == k2 && abilities1 > abilities2) || (k == k2 && abilities1 == abilities2 && rand > 0.5)
        then do
        let newNinja = checkJourneyman(Ninja a b c d e f g (h+1) (k+10))
        let newlists = addNinjaToList newLists newNinja
        print newNinja
        return newlists
        else do--if k < k2 || (k == k2 && abilities2 > abilities1) || (k == k2 && abilities1 == abilities2 && rand <= 2)
        let newNinja = checkJourneyman(Ninja a2 b2 c2 d2 e2 f2 g2 (h2+1) (k2+10))
        let newlists = addNinjaToList newLists newNinja
        print newNinja
        return newlists
        
        
        
--Returns the final allLists and the Ninja that is deleted
findCountry :: String -> String -> [[Ninja]] -> IO ([[Ninja]], Ninja)
findCountry country name allLists@[fi,l,wi,wa,ea]
  | country == "F" || country == "f" = do
  x <- deleteNinja name fi
  if length (fst x) == length fi
    then do return(allLists, snd x)
    else return ([fst x,l,wi,wa,ea], snd x)

  | country == "W" || country == "w" = do
  x <- deleteNinja name wa
  if length (fst x) == length wa
    then do return(allLists, snd x)
    else return (([fi,l,wi,fst x,ea], snd x))
  
  | country == "N" || country == "n" = do
  x <- deleteNinja name wi
  if length (fst x) == length wi
    then do return(allLists, snd x)
    else return (([fi,l,fst x,wa,ea], snd x))

  | country == "E" || country == "e" = do
  x <- deleteNinja name ea
  if length (fst x) == length ea
    then do return(allLists, snd x)
    else return (([fi,l,wi,wa,fst x], snd x))
  
  | country == "L" || country == "l" = do
  x <- deleteNinja name l
  if length (fst x) == length l
    then do return(allLists, snd x)
    else return (([fi,fst x,wi,wa,ea], snd x))
    
  | otherwise = return (([fi,l,wi,wa,ea], Ninja "No Country" 'f' "" 0.0 0.0 "" "" 0 0.0))
  
  
deleteNinja :: String -> [Ninja] -> IO ([Ninja], Ninja)
deleteNinja name x@(nin@(Ninja a b c d e f g h k):xs)
  | name == a     = return(xs,nin)
  | name /= a     = do
                    if length xs == 0
                      then return(x, Ninja "No Ninja" 'f' "" 0.0 0.0 "" "" 0 0.0)
                    else do
                    res <- deleteNinja name xs
                    return (nin:(fst $ res ), (snd $ res))
                    

giveMeCorrectList :: String -> [[Ninja]] -> [Ninja]
giveMeCorrectList country allLists@[fi,l,wi,wa,ea] 
  | country == "F" || country == "f" = fi
  | country == "L" || country == "l" = l
  | country == "N" || country == "n" = wi
  | country == "W" || country == "w" = wa
  | country == "E" || country == "e" = ea
  
  
printJourneyman ::  [Ninja] -> Bool -> IO ([Ninja],Bool)
printJourneyman [] _ = return ([], False)
printJourneyman countryList@(x:xs) isForExit = do
  let firstNinja@(Ninja a b s d e f g r k) = x
  if r == 3
  then do
    if (isForExit == True)
      then do print (firstNinja)
      else do return ()
    return (countryList,True)
  else do
  printJourneyman xs False
  
  
 
checkJourneyman :: Ninja -> Ninja
checkJourneyman nin@(Ninja a b s d e f g r k)
  | r==3  = Ninja a b "Journeyman" d e f g r k
  | r /=3 = nin
  
  
--Each Ninja's countries checked then added country array list
addNinjaToList :: [[Ninja]] -> Ninja -> [[Ninja]]
addNinjaToList allLists@[fi,l,wi,wa,ea] nin@(Ninja a b c d e f g h k) = case b of
  'F' -> [sortNinjas (fi ++ [nin]),l,wi,wa,ea]
  'L' -> [fi,sortNinjas(l ++ [nin]),wi,wa,ea]
  'N' -> [fi,l,sortNinjas(wi ++ [nin]),wa,ea]
  'W' -> [fi,l,wi,sortNinjas(wa ++ [nin]),ea]
  'E' -> [fi,l,wi,wa,sortNinjas(ea ++ [nin])]
  _   -> allLists
  
  
  
  
  
---------------------------------------------------------Functions in Part D--------------------------------------------------------------------


makeRoundCountries :: [[Ninja]] -> IO [[Ninja]]
makeRoundCountries allLists = do
  putStrLn "Enter the first country code: "
  country1 <- getLine
  putStrLn "Enter the second country code: "
  country2 <- getLine
  (newLists, ninja1@(Ninja a b c d e f g h k)) <- getFirstNinja country1 allLists
  (newLists, ninja2@(Ninja a2 b2 c2 d2 e2 f2 g2 h2 k2)) <- getFirstNinja country2 newLists
  --Both ninjas have been deleted. After the comparison, winner ninja will be added again.

  
  let array1 = giveMeCorrectList country1 allLists
  let array2 = giveMeCorrectList country2 allLists
    
  (_,canFight1) <- printJourneyman array1 False
  (_,canFight2) <- printJourneyman array2 False
  if (canFight1 == True)
    then do putStrLn (charToCountry b ++ " country cannot be included in a fight\n")
            return allLists
  else if (canFight2 == True) 
    then do putStrLn (charToCountry b2 ++ " country cannot be included in a fight\n")
            return allLists
  else do
    let abilities1 = calculateAbility f + calculateAbility g
    let abilities2 = calculateAbility f2 + calculateAbility g2
    rand <- drawDouble 0.0 1.0
    putStr ("Winner: ")



    --Compare scores and add the winner ninja to the list with updated values
    if k > k2 || (k == k2 && abilities1 > abilities2) || (k == k2 && abilities1 == abilities2 && rand > 0.5)
      then do
      let newNinja = checkJourneyman (Ninja a b c d e f g (h+1) (k+10))
      let newlists = addNinjaToList newLists newNinja
      print ( newNinja )
      return newlists
      else do--if k < k2 || (k == k2 && abilities2 > abilities1) || (k == k2 && abilities1 == abilities2 && rand <= 2)
      let newNinja = checkJourneyman (Ninja a2 b2 c2 d2 e2 f2 g2 (h2+1) (k2+10))
      let newlists = addNinjaToList newLists newNinja
      print (newNinja)
      return newlists




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
  
  
  
  
  
---------------------------------------------------------Functions in Part E--------------------------------------------------------------------


countryJourneymans :: [[Ninja]] -> IO [[Ninja]]
countryJourneymans allLists@(x:xs)
  | length xs == 0 = do
    printJourneyman x True
    return [[]]
  | otherwise = do
    printJourneyman x True
    countryJourneymans xs
    return allLists
    
    
    
    
    

-------------------------------------------------------SMALL FUNCTIONS----------------------------------------------------
-- Ninja's total score is calculated
putScore :: Ninja -> Ninja
putScore nin@(Ninja name county stat exam1 exam2 ab1 ab2 r score) =
  Ninja name county stat exam1 exam2 ab1 ab2 r (0.5*exam1 + 0.3*exam2 + calculateAbility ab1 + calculateAbility ab2 + 10* (fromIntegral r))



drawDouble :: Double -> Double  -> IO Double
drawDouble x y = getStdRandom (randomR (x,y))




instance Show Ninja where
  show (Ninja a b c d e f g h k) = a ++ ", Score: " ++  show k ++ ", Status: " ++ c ++ ", Round: " ++ show h ++ "\n"



charToCountry :: Char -> String
charToCountry c
  | c == 'F' || c == 'f' = "Fire"
  | c == 'W' || c == 'w' = "Water"
  | c == 'N' || c == 'n' = "Wind"
  | c == 'E' || c == 'e' = "Earth"
  | c == 'L' || c == 'l' = "Lightning"




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





-------------------------------------------------------FUNCTIONS FOR INPUT READING----------------------------------------------------
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









  












