module LogParser where

-- functions to break the log file into useful, typed, data
-- this data will be shaped by FatesUnit and displayed in Main

-- right now it just breaks it up into strings, though

toCharacters :: String -> [[String]]
toCharacters logString = map (drop 1) characterChunks
  where characterChunks = (group 13 . drop 1 . lines) logString
  -- breaks into lines, drops the first (this contains the seed)
  -- the groups the rest into 12-length chunks before removing newlines
    

-- returns tuple of (character, replaced)
-- where "character" is the unit that is replacing "replaced"
toSwap :: [String] -> (String, String)
toSwap characterChunk = (charName, replacedName)
  where
    charName     = drop 6  $ head characterChunk -- "Name: " from first line
    replacedName = drop 11 $ (head . tail) characterChunk -- "Replacing: " from second line 




-- breaks a list into chunks of length n
group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative or zero chunk length"

  
