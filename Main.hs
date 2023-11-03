import Data.List

import LogParser
import FatesUnit


-- currently just
-- asks for an input file
-- outputs which characters are parents

-- next steps involve outputting which units they are parents to

main :: IO ()
main = do
  putStrLn "Please enter the name of the output file."

  inputFile <- getLine
  logText   <- readFile inputFile

  let characters = toCharacters logText
      swaps      = map toSwap characters
      parents    = filter isParent swaps
        
  mapM_ (putStrLn . fst) parents


