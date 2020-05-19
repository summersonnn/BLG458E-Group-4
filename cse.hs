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

  contents <- hGetContents output
  --hGetContents :: Handle -> IO String
  createNinjas contents
  hClose output

createNinjas :: String -> IO()
createNinjas contents = do
  let allLines = lines contents
  --lines :: String -> [String] (creates an array of string from the original one, new line characters serving as separators )
  print (allLines !! 0)
  --Her bir satır. su an allLines listesinin içinde bir eleman.
  --Bu elemanları name. country şeklinde parse edip ninja oluşturacağız
  --Fonksiyonun return type'ı debug amaclı print yapabilmek için simdilik IO()

  --ornek dummy ninja
  let sasuke = Ninja "sasuke" 'c' "fsdf" 33 22 "fds" "dsdf" 33 33
  --ilk satırı kelimelere bolmek için string arrayine donusturuyor
  let firstLineAsList = convertLineToList (allLines !! 0)
  --verilen arrayden ninja olusturuyor
  let firstNinja = createOneNinja(firstLineAsList)
  --printlemeyi daha guzel yapmak amacıyla ninja için show fonksyionu yazılmalı
  print(firstNinja)
  --her bir satırı recursive olarak çağıracak ve ninjaya donusturecek bir fonksiyon yazılabilir




data Ninja = Ninja {name:: String, country:: Char,
status:: String, exam1:: Float,
exam2:: Float, ability1:: String,
ability2:: String, r:: Int,
score:: Int} deriving Show

--kelime listesinden sırayla alıp gerekli parametre olarak ninjaya veriyor
--stringleri floata donusturmek icin read fonksiyonu kullandım
createOneNinja :: [String] -> Ninja
createOneNinja line = Ninja (line !! 0) ((line !! 1) !! 0) "" (read (line !! 2)) (read (line !! 3)) (line !! 4) (line !! 5) 0 0

--tek bir stringi(cümleyi) kelimelerden olusan arraye donusturuyor
convertLineToList :: String -> [String]
convertLineToList givenLine =
  splitOn " " (givenLine)
