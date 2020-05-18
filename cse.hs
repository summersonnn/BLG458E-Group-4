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
	putStr contents  
	--String -> IO ()
	hClose output
    
 
data Ninja = Ninja {name:: String, country:: Char,
status:: String, exam1:: Float,
exam2:: Float, ability1:: String,
ability2:: String, r:: Int,
score:: Int}   
    

