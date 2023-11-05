module LogParser where

-- functions to break the log file into useful, typed, data
-- that is, strings and [ints] and [doubles] to be passed to constructors in FatesUnit

-- this data will be shaped by FatesUnit and displayed in Main

-- right now it just breaks it up into strings, though

type CharacterChunk = [String]

toCharacters :: String -> [CharacterChunk]
toCharacters logString = map (drop 1) characterChunks
  where characterChunks = (group 13 . drop 1 . lines) logString
  -- breaks into lines, drops the first (this contains the seed)
  -- the groups the rest into 13-length chunks before removing newlines
    

-- get name of unit from a characterchunk
unitName :: CharacterChunk -> String
unitName = (drop 6 . head)

-- gets list of classes a character has access to
-- in order of primary, secondary, reclasses
-- FatesUnit will do work of grabbing whichever is basic class from (primary, secondary)
-- and will also scrub Nones from reclasses. we are only manipulating strings here
classList :: CharacterChunk -> [String]
classList characterChunk = primaryClass:secondaryClass:reclasses
  where
    primaryClass   = drop 15 (characterChunk !! 2) -- drop "Primary Class: " from third line
    secondaryClass = drop 17 (characterChunk !! 3) -- drop "Secondary Class: " from fourth line
    reclasses = [firstReclass, drop 2 secondReclass]
    -- characters have at most two reclasses, either of which may be None
    (firstReclass, secondReclass) = (break (==',') . drop 11) (characterChunk !!  4) -- drop "Reclasses: " and then break at comma
    

-- gets lists of skills a character starts with
-- a character has one personal skill and then some number of equipped skills
skillList :: CharacterChunk -> (String, [String])
skillList characterChunk = (personal, map (dropWhile (==' ')) equipped)

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

-- returns tuple of (character, replaced)
-- where "character" is the unit that is replacing "replaced"
toSwap :: CharacterChunk -> (String, String)
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

  
