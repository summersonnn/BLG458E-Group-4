-- compile and run
-- ghc cse.hs -o cse
-- ./cse csereport.txt


import System.IO
import System.Environment
import Data.List
import Data.List.Split
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
  --converts cse.report.txt file to string
  let allLines      = lines contents
  --create array of countries and fill it with the data given in
  --listsCombined is an array of Ninja arrays. Each Ninja array represents a country and all lists represents all countries
  listsCombined <- fillLists allLines [fire,lightning,wind,water,earth]

  --call main function and give the all data
  xfunc listsCombined

  --close file
  hClose output



--function to convert the data(in string form) to [[Ninja]] form.
--This way we have lists of countries and each country has lists of Ninjas
--takes data as string and array to fill
--each line corresponds to one Ninja
fillLists :: [String] -> [[Ninja]] -> IO [[Ninja]]
fillLists allLines x@[fire,lightning,wind,water,earth] = do
  let oneLine   = getOneLine allLines --takes the first line of allLists
  if oneLine == "" then return x
  else do
  let aninja    = lineToNinja oneLine --converts the given line(String) to Ninja type
  let newx      = addNinjaToList x aninja     --((fire ++ [aninja]):water)
  fillLists (tail allLines) newx --continue with the next line and give the newly added List as parameter


--Main Function for the code, keep updates the All Lists array
--Ninja array'lerinden oluşan array alır. Hem IO yapar hem Ninja array'lerinden oluşan array döndürür.
xfunc :: [[Ninja]] -> IO [[Ninja]]--gets AllLists and returns the updated list(if there is a change)
xfunc allLists = do
  putStrLn "a) View a Country's Ninja Information"
  putStrLn "b) View All Countries' Ninja Information"
  putStrLn "c) Make a Round Between Ninjas"
  putStrLn "d) Make a Round Between Countries"
  putStrLn "e) Exit"
  userSelection <- getLine
  if userSelection == "e"
  then do
    --print the journeymans and exit
    countryJourneymans allLists
    return allLists
  else if userSelection == "a"
  then do
    getCountry allLists--getCountry gets the country code and prints the country list
    xfunc allLists--return the main function again
  else if userSelection == "b"
  then do
    printAllCountries allLists-- this function gets the array which has the all Ninjas
    xfunc allLists--return the main function again
  else if userSelection == "c"
  then do
    allLists <- makeRoundNinjas allLists --call function to fight
    xfunc allLists--return the main function again
  else if userSelection == "d"
  then do
    allLists <- makeRoundCountries allLists--call function to fight
    xfunc allLists--return the main function again
  else do--if there is invalid input(other than a,b,c,d) return itself and ask selection again
    xfunc allLists--return the main function again




-------------------------------------------------------Functions in Part A----------------------------------------------------------------------
--gets country selection and calls printcountry
getCountry :: [[Ninja]] -> IO [[Ninja]]
getCountry allLists = do
  putStrLn "Enter the country code: "
  countryCode <- getLine--gets input from user to get the country selection
  printCountry allLists countryCode--call function to print ninjas in country
  return allLists -- return the same allLists without changing anything


--according to the country code given call showZ function to this particular country
printCountry :: [[Ninja]] -> String -> IO ()--gets allList and country code
printCountry allLists@[fi,l,wi,wa,ea] country-- get allList and split them into countries
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
    print "Invalid country code"--show invalid input error


--custom sorting function
--gets two ninjas compare their rounds and if they are equal compare their score
sortbyRS nin1@(Ninja a b s d e f g round1 score1) nin2@(Ninja a2 b2 s2 d2 e2 f2 g2 round2 score2)
  | round1 > round2 = False
  | round1 < round2 = True
  | round1 == round2 = (score2 < score1)

--insertion sort
--higher order function is used here
--as input gets function to compare elements(ninja in this case but can be used for other types)
--as another inputs it takes an element and a list then push the element in the right place in the ordered list
--returns inserted list
ins :: (a -> a -> Bool) -> a -> [a] -> [a]
ins p n [] = [n]--if the list is empty just add the element
ins p n xs@(xu:xsu)
--compare n with the first element using the comparing function p
  | p n xu = n : xs--if it is in the right place add it as the first element to the remaining list and return
  | otherwise = xu : ins p n xsu--if it is not in the right place continue looking in the remaining list

--sort whole list
--higher order function is used
--as input gets function to compare elements
--gets list to sort and returns ordered list
iSort :: (a -> a -> Bool) -> [a] -> [a]
iSort p [] = []--if list is empty return empty list
iSort p (x:xs) = ins p x (iSort p xs)--insert each element to the sorted list beggining from an empty list




--------------------------------------------------------Functions in Part B--------------------------------------------------------------------

--print all ninjas in the list
printAllCountries :: [[Ninja]] -> IO [[Ninja]]
printAllCountries allLists--gets array of ninja array
  | length allLists == 0 = do --if list is empty print nothing and return the given list
      putStrLn ""
      return allLists
  | length allLists /= 0 = do
      putStr $ showZ (head allLists)--print the first country
      printAllCountries (tail allLists)-- continue with the remaining countries



showZ :: Show a => [a] -> String--we make sure that type a can be applied to the show function
--map is a higher order function it applies a function to every element in the list
--in this case we apply show function to every ninja in the list
showZ = intercalate "" . map show

---------------------------------------------------------Functions in Part C--------------------------------------------------------------------
makeRoundNinjas :: [[Ninja]] -> IO [[Ninja]]--fighting function between ninjas
makeRoundNinjas allLists = do--getss all ninjas
--inputs
  putStrLn "Enter the name of the first ninja: "
  firstName <- getLine
  putStrLn "Enter the country code of the first ninja: "
  firstCountry <- getLine
  putStrLn "Enter the name of the second ninja: "
  secondName <- getLine
  putStrLn "Enter the country code of the second ninja: "
  secondCountry <- getLine
  -- call find country function. It returns the founded Ninja and the country it belongs to.
  (newLists, ninja1@(Ninja a b c d e f g h k)) <- findCountry firstCountry firstName allLists
  (newLists, ninja2@(Ninja a2 b2 c2 d2 e2 f2 g2 h2 k2)) <- findCountry secondCountry secondName newLists
  --Both ninjas have been deleted. After the comparison, winner ninja will be added again.

  --input controll
  if (a == "No Ninja" || a2 == "No Ninja")
    then do putStrLn "No such Ninja. Please make sure you type the name of the Ninja correctly.\n"
            return allLists
    else if (a == "No Country" || a2 == "No Country")
    then do putStrLn "No such Country. Please make sure you type the name of the country correctly.\n"
            return allLists
    else do--inputs are correct

   --check condition where ninja is already a journeyman or country has already a journeyman
   --function composition is used here
   --giveMeCorrectList function takes two parameters: firstCountry and allLists and outputs [Ninja] which is the corresponding list for typed country.
   --printJourneyman takes corresponding list and "False" argument. "False" argument is for not printing. If it was set to True, it would print in it.
   --To see how the .... operator worksi please look at line 231
   --printJourneyman outputs a tuple whose second value is Bool. So we don't care about the first value and store the second value in canFight
      (_,canFight1) <- (printJourneyman .... giveMeCorrectList) firstCountry allLists False
      (_,canFight2) <- (printJourneyman .... giveMeCorrectList) secondCountry allLists False

      --error messages for journeyman check
      if (canFight1 == True)
        then do putStrLn (charToCountry b ++ " country cannot be included in a fight\n")
                return allLists
      else if (canFight2 == True)
        then do putStrLn (charToCountry b2 ++ " country cannot be included in a fight\n")
                return allLists
      else do--do fight
      --calculate ability points
        let abilities1 = calculateAbility f + calculateAbility g
        let abilities2 = calculateAbility f2 + calculateAbility g2
        putStr "Winner: "
        --Compare scores and add the winner ninja to the list with updated values
        if k > k2 || (k == k2 && abilities1 > abilities2)--first ninja wins
        then do
          --winner ninja's round number and score is updated
        let newNinja = checkJourneyman(Ninja a b c d e f g (h+1) (k+10))--this function makes the title "journeyman" if round number is 3
        let newlists = addNinjaToList newLists newNinja
        print newNinja--print winner ninja
        return newlists--this list does not contain the loser ninja

        else do--if k < k2 || (k == k2 && abilities2 > abilities1) second ninja wins
        --winner ninja's round number and score is updated
        let newNinja = checkJourneyman(Ninja a2 b2 c2 d2 e2 f2 g2 (h2+1) (k2+10))--this function makes the title "journeyman" if round number is 3
        let newlists = addNinjaToList newLists newNinja
        print newNinja--print winner ninja
        return newlists--this list does not contain the loser ninja

--Function composition.
--Function g takes two parameters x and y, outputs single value.
--Function f takes the output of the function g and another q as parameter
(....) :: (z -> q -> c) -> (a -> b -> z) -> a -> b -> q -> c
(....) f g x y q = f (g x y) q

--used for match
--find ninja and delete it from the list
--Returns the final allLists and the Ninja that is deleted
--gets ninjas name country code and allLists
findCountry :: String -> String -> [[Ninja]] -> IO ([[Ninja]], Ninja)
findCountry country name allLists@[fi,l,wi,wa,ea]
  --check countries with the given country code
  | country == "F" || country == "f" = do
  x <- deleteNinja name fi--delete found ninja
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

--gets ninja name and country list, returns the ninja and country list without the found ninja
deleteNinja :: String -> [Ninja] -> IO ([Ninja], Ninja)
deleteNinja name x@(nin@(Ninja a b c d e f g h k):xs)
  | name == a     = return(xs,nin)--ninja is found, add the rest of the list except the current(found) ninja
  | name /= a     = do--ninja is not found yet
                    if length xs == 0--at the end of the list if still ninja is not found give error message
                      then return(x, Ninja "No Ninja" 'f' "" 0.0 0.0 "" "" 0 0.0)
                    else do--calls itself and searchs ninja in the rest of the list
                    res <- deleteNinja name xs
                    return (nin:(fst $ res ), (snd $ res))

--gives the country list according to given country code
giveMeCorrectList :: String -> [[Ninja]] -> [Ninja]
giveMeCorrectList country allLists@[fi,l,wi,wa,ea]
  | country == "F" || country == "f" = fi
  | country == "L" || country == "l" = l
  | country == "N" || country == "n" = wi
  | country == "W" || country == "w" = wa
  | country == "E" || country == "e" = ea


--controlls if the ninja is a journeyman
--if journeyman, it is printed
--this function is used for option e
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
  printJourneyman xs isForExit


--if the round number is equal to 3 function changes the status to "Journeyman" and returns the ninja
checkJourneyman :: Ninja -> Ninja
checkJourneyman nin@(Ninja a b s d e f g r k)
  | r==3  = Ninja a b "Journeyman" d e f g r k
  | r /=3 = nin


--Each Ninja's countries checked then added country array list
addNinjaToList :: [[Ninja]] -> Ninja -> [[Ninja]]
addNinjaToList allLists@[fi,l,wi,wa,ea] nin@(Ninja a b c d e f g h k) = case b of
  'F' -> [iSort sortbyRS (fi ++ [nin]),l,wi,wa,ea]
  'L' -> [fi,iSort sortbyRS (l ++ [nin]),wi,wa,ea]
  'N' -> [fi,l,iSort sortbyRS (wi ++ [nin]),wa,ea]
  'W' -> [fi,l,wi,iSort sortbyRS (wa ++ [nin]),ea]
  'E' -> [fi,l,wi,wa,iSort sortbyRS (ea ++ [nin])]
  _   -> allLists


---------------------------------------------------------Functions in Part D--------------------------------------------------------------------

--gets allList and returns updated allLists
makeRoundCountries :: [[Ninja]] -> IO [[Ninja]]
makeRoundCountries allLists = do
  --IO operations
  putStrLn "Enter the first country code: "
  country1 <- getLine
  putStrLn "Enter the second country code: "
  country2 <- getLine
  --find ninjas and their country lists
  (newLists, ninja1@(Ninja a b c d e f g h k)) <- getFirstNinja country1 allLists
  (newLists, ninja2@(Ninja a2 b2 c2 d2 e2 f2 g2 h2 k2)) <- getFirstNinja country2 newLists
  --Both ninjas have been deleted. After the comparison, winner ninja will be added again.

  --input check for valid names and countries
  if (a == "No Ninja" || a2 == "No Ninja")
    then do putStrLn "No such Ninja. Please make sure the country has a ninja.\n"
            return allLists
  else if (a == "No Country" || a2 == "No Country")
    then do putStrLn "No such Country. Please make sure you type the name of the country correctly.\n"
            return allLists
  else do--check journeyman prerequest
  --check condition where ninja is already a journeyman or country has already a journeyman
  --function composition. To see detailed explanationi have a look at line 193 (same thing is done there)
  (_,canFight1) <- (printJourneyman .... giveMeCorrectList) country1 allLists False
  (_,canFight2) <- (printJourneyman .... giveMeCorrectList) country2 allLists False
  if (canFight1 == True)
    then do putStrLn (charToCountry b ++ " country cannot be included in a fight\n")
            return allLists
  else if (canFight2 == True)
    then do putStrLn (charToCountry b2 ++ " country cannot be included in a fight\n")
            return allLists
  else do--calculate ability points
    let abilities1 = calculateAbility f + calculateAbility g
    let abilities2 = calculateAbility f2 + calculateAbility g2
    putStr ("Winner: ")



    --Compare scores and add the winner ninja to the list with updated values
    if k > k2 || (k == k2 && abilities1 > abilities2)
      then do
      let newNinja = checkJourneyman (Ninja a b c d e f g (h+1) (k+10))--this function makes the title "journeyman" if round number is 3
      let newlists = addNinjaToList newLists newNinja
      print ( newNinja )--print winner
      return newlists
      else do--if k < k2 || (k == k2 && abilities2 > abilities1)
      let newNinja = checkJourneyman (Ninja a2 b2 c2 d2 e2 f2 g2 (h2+1) (k2+10))--this function makes the title "journeyman" if round number is 3
      let newlists = addNinjaToList newLists newNinja
      print (newNinja)--print winner
      return newlists

      --returns updated list: loser ninja is removed and winner ninja is updated with new score/round/status




--Returns the final allLists(after substracting the first ninja from the given country) and deleted Ninja
getFirstNinja :: String -> [[Ninja]] -> IO ([[Ninja]], Ninja)
getFirstNinja country allLists@[fi,l,wi,wa,ea]
  | country == "F" || country == "f" = do
  if length fi > 0
    then do return ([tail fi,l,wi,wa,ea], head fi)
  else do return ([tail fi,l,wi,wa,ea], Ninja "No Ninja" 'f' "" 0.0 0.0 "" "" 0 0.0)

  | country == "W" || country == "w" = do
  if length wa > 0
    then do return ([fi,l,wi,tail wa,ea], head wa)
  else do return ([fi,l,wi,tail wa,ea], Ninja "No Ninja" 'f' "" 0.0 0.0 "" "" 0 0.0)

  | country == "N" || country == "n" = do
  if length wi > 0
    then do return ([fi,l,tail wi,wa,ea], head wi)
  else do return ([fi,l,tail wi,wa,ea], Ninja "No Ninja" 'f' "" 0.0 0.0 "" "" 0 0.0)

  | country == "E" || country == "e" = do
  if length ea > 0
    then do return ([fi,l,wi,wa,tail ea], head ea)
  else do return ([fi,l,wi,wa,tail ea], Ninja "No Ninja" 'f' "" 0.0 0.0 "" "" 0 0.0)

  | country == "L" || country == "l" = do
  if length l > 0
    then do return ([fi,tail l,wi,wa,ea], head l)
  else do return ([fi,tail l,wi,wa,ea], Ninja "No Ninja" 'f' "" 0.0 0.0 "" "" 0 0.0)

  | otherwise = do return ([fi,l,wi,wa,ea], Ninja "No Country" 'f' "" 0.0 0.0 "" "" 0 0.0)



---------------------------------------------------------Functions in Part E--------------------------------------------------------------------

--check all ninjas and print the journeymans
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

--Show function for Ninja type is
--this function is called everytime we want to print/show a Ninjas
--only name score status and round is printed
instance Show Ninja where
  show (Ninja a b c d e f g h k) = a ++ ", Score: " ++  show k ++ ", Status: " ++ c ++ ", Round: " ++ show h ++ "\n"


--gets country code and returns country name
charToCountry :: Char -> String
charToCountry c
  | c == 'F' || c == 'f' = "Fire"
  | c == 'W' || c == 'w' = "Water"
  | c == 'N' || c == 'n' = "Wind"
  | c == 'E' || c == 'e' = "Earth"
  | c == 'L' || c == 'l' = "Lightning"



--ability point definitions
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

--Ninja datatype
data Ninja = Ninja {name:: String, country:: Char,
status:: String, exam1:: Float,
exam2:: Float, ability1:: String,
ability2:: String, r:: Int,
score:: Float}

--initial empty country lists
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
