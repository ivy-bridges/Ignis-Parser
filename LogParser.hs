module LogParser where

-- functions to break the log file into useful, typed, data
-- that is, strings and [ints] and [doubles] to be passed to constructors in FatesUnit



-- characters in the log file are represented by blocks of text
type CharacterChunk = [String]

toCharacters :: String -> [CharacterChunk]
toCharacters logString = map (drop 1) characterChunks
  where characterChunks = (group 13 . drop 1 . lines) logString
  -- breaks into lines, drops the first (this contains the seed)
  -- the groups the rest into 13-length chunks before removing newlines
    

-- get name of unit and replaced from a characterchunk
readName :: CharacterChunk -> String
readName = (drop 6 . head)

readReplaced :: CharacterChunk -> String
readReplaced = (drop 11 . head . tail)

-- gets list of classes a character has access to
-- in order of primary, secondary, reclasses
readClasses :: CharacterChunk -> [String]
readClasses characterChunk = primaryClass:secondaryClass:reclasses
  where
    primaryClass   = drop 15 (characterChunk !! 2) -- drop "Primary Class: " from third line
    secondaryClass = drop 17 (characterChunk !! 3) -- drop "Secondary Class: " from fourth line
    reclasses = [firstReclass, drop 2 secondReclass]
    -- characters have at most two reclasses
    (firstReclass, secondReclass) = (break (==',') . drop 11) (characterChunk !!  4) -- drop "Reclasses: " and then break at comma
    

-- gets lists of skills a character starts with
-- a character has one personal skill and then some number of equipped skills
readSkills :: CharacterChunk -> (String, [String])
readSkills characterChunk = (personal, map (dropWhile (==' ')) equipped)
  where personal = drop 16 (characterChunk !! 5) -- drop "Personal Skill: "
        -- we move through the (string) list of equipped skills to make a [string]
        -- ignoring ' quotes and [] brackets, and breaking into new values at , commas
        -- breaking like this requires removing a leading space from the start of all non-head skills
        equipped = (foldr skillFold [""] . drop 14) (characterChunk !! 6)
        skillFold x acc
          | x == ','      = "":acc
          | ignoredChar x = acc
          | otherwise     = (x:head acc):(tail acc)
          where ignoredChar x = elem x "'[]"

-- get base stats and growths of a character
readBases :: CharacterChunk -> [Int]
readBases characterChunk = read $ drop 7 (characterChunk !! 7)

readGrowths :: CharacterChunk -> [Double]
readGrowths characterChunk = read $ drop 9 (characterChunk !! 8)

-- returns tuple of (character, replaced)
-- where "character" is the unit that is replacing "replaced"
toSwap :: CharacterChunk -> (String, String)
toSwap characterChunk = (readName characterChunk, readReplaced characterChunk)



-- breaks a list into chunks of length n
group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative or zero chunk length"

  
