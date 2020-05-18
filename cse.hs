import System.IO  
import System.Environment
import Data.List 

  
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
				
    
 
data Ninja = Ninja {name:: String, country:: Char,
status:: String, exam1:: Float,
exam2:: Float, ability1:: String,
ability2:: String, r:: Int,
score:: Int}   
    

