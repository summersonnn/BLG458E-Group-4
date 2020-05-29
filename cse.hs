--compile and run
-- ghc cse.hs -o cse
-- ./cse csereport.txt

import System.IO
import System.Environment
import Data.List
import Data.List.Split



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

  xfunc listsCombined
  {-let oneLine   = getOneLine allLines
  let aninja    = lineToNinja oneLine

  let a    = tail (tail (tail allLines))
  let b    = getOneLine a
  let c    = lineToNinja b

  let x    = tail (tail (tail a))
  let y    = getOneLine x
  let z    = lineToNinja y-}

  --smth <- xfunc [fire]
  --print smth

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
  then return allLists
  else if userSelection == "b"
  then do
    print allLists
    xfunc allLists
  else xfunc allLists




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
  [x]    -> x
  (x:_)  -> x

getOneLineFromEnd :: [String] -> String
getOneLineFromEnd y@(x:xs) = case y of
  []       -> ""
  [x]      -> x
  (x:xs)   -> getOneLineFromEnd(xs)

--Kelimelerin olduğu arrayi (string arrayi) tek bir ninjaya çevirir
createOneNinja :: [String] -> Ninja
createOneNinja line
  | (line !! 1) == "Wind"    = Ninja (line !! 0) 'N' "Junior" (read (line !! 2)) (read (line !! 3)) (line !! 4) (line !! 5) 0 0
  | (line !! 1) /= "Wind"    = Ninja (line !! 0) ((line !! 1) !! 0) "Junior" (read (line !! 2)) (read (line !! 3)) (line !! 4) (line !! 5) 0 0





--tek bir stringi(cümleyi) kelimelerden olusan arraye donusturuyor
convertLineToList :: String -> [String]
convertLineToList givenLine =
  splitOn " " (givenLine)



data Ninja = Ninja {name:: String, country:: Char,
status:: String, exam1:: Float,
exam2:: Float, ability1:: String,
ability2:: String, r:: Int,
score:: Int}

instance Show Ninja where
  show (Ninja a b c d e f g h k) = show a ++ ", Score: " ++  show k ++ ", Status: " ++ show c ++ ", Round: " ++ show h ++ "\n"



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


{-deneme :: [String] -> [String]
deneme allLines@(x:xs) = do
  let words = convertLineToList(x)
  let aninja = createOneNinja(words)
  fire ++ [aninja]
  return (xs)-}


-- fail zeynep denemeleri
  {-buffer :: IO [[Ninja]] -> [[Ninja]]
  buffer allLists =
    buffer allLists

  getCountry :: [[Ninja]] -> IO [[Ninja]]
  getCountry allLists = do
    putStrLn "Enter the country code: "
    country <- getLine
    putStrLn (printCountry allLists country)
    return allLists

  printCountry :: [[Ninja]] -> String -> String
  printCountry allLists country
    | country == "f" = "Fire \n"
  --  | _              = "no country choosen"

  -}
