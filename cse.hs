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
  let oneLine = getOneLine (lines contents)
  let words   = convertLineToList (oneLine)
  let aninja  = createOneNinja(words)
  --fire ++ [aninja]   IO tipi (IO Ninja) dönmediği için hata veriyor. hem IO tipinde döndürüp hem listeye nasıl ekleriz?
  print aninja
  hClose output

--Her bir satırı array'e farklı eleman olarak dizilmiş string arrayinden ilk elemanı (ilk satırı) getirir
getOneLine :: [String] -> String
getOneLine allLines = case allLines of
  []     -> ""
  (x:_)  -> x
  
--Kelimelerin olduğu arrayi (string arrayi) tek bir ninjaya çevirir
createOneNinja :: [String] -> Ninja
createOneNinja line = Ninja (line !! 0) ((line !! 1) !! 0) "" (read (line !! 2)) (read (line !! 3)) (line !! 4) (line !! 5) 0 0

--tek bir stringi(cümleyi) kelimelerden olusan arraye donusturuyor
convertLineToList :: String -> [String]
convertLineToList givenLine =
  splitOn " " (givenLine)
  
  
{-var
	s: Char
addNinjaToList :: Ninja -> [Ninja]
addNinjaToList nin
  s:= nin.country
  | s  == 'f' =  [nin] ++ fire
  | s  == 'l' =  [nin] ++ lightning
  | s  == 'w' =  [nin] ++ water
  | s  == 'e' =  [nin] ++ earth-}
  
  
data Ninja = Ninja {name:: String, country:: Char,
status:: String, exam1:: Float,
exam2:: Float, ability1:: String,
ability2:: String, r:: Int,
score:: Int} deriving Show

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
