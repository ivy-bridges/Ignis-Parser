import Data.List
import Control.Monad
import Data.Maybe
import Data.Tuple (swap)

import LogParser
import FatesUnit
import FatesClass (readClass)


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
      parents    = filter (isParent . fst) swaps
        
  forM_ characters (\c -> do
                       putStrLn (readName c)
                       mapM_ putStrLn (readClasses c)
                       putStrLn ""
                   )



-- constructs a FatesUnit based on info retrieved by LogParser
readChunk :: CharacterChunk -> FatesUnit
readChunk charChunk = readUnit name classes skills bases growths
  where
    name    = readName charChunk
    classes = (catMaybes . map readClass) (readClasses charChunk)  -- grab non-None classes
    skills  = readSkills charChunk
    bases   = readBases charChunk
    growths = readGrowths charChunk


loadReport :: String -> IO [CharacterChunk]
loadReport inputFile = do
  logText <- readFile inputFile
  return $ toCharacters logText



-- find corresponding child, if one exists
findChild :: [CharacterChunk] -> String -> Maybe String
findChild charList unitName = childSwap
  where
    replaced
      | elem unitName ["Corrin (M)", "Corrin (F)"] = Just unitName -- never swapped
      | otherwise = lookup unitName (map toSwap charList) -- find the unit they replace
    childOrig = replaced  >>= (flip lookup basePairs) -- find that unit's child
    childSwap = childOrig >>= (flip lookup (map (swap . toSwap) charList)) -- find who replaced them

-- finds child's FatesUnit, if one exists
findChildUnit :: [CharacterChunk] -> FatesUnit -> Maybe FatesUnit
findChildUnit charList unit = childUnit
  where
    unitName   = getName unit
    charTable  = map (\c -> (readName c, readChunk c)) charList
    childUnit  = (findChild charList unitName) >>= (flip lookup charTable)
  

-- produces a list of all (parent, offspring) pairs in a randomizer
-- this is what it was all for
findFamilies :: [CharacterChunk] -> [(FatesUnit, FatesUnit)]
findFamilies charList = unitFamilies
  where
    unitList      = map readChunk charList
    maybePairings = map (\unit -> (unit, findChildUnit charList unit)) unitList -- tries to find pairings for all units
    validPairings = filter (isJust . snd) maybePairings   -- where child actually exists
    unitFamilies  = map (\(parent, child) -> (parent, fromJust child)) validPairings
    


-- allUnits :: [(String, FatesUnit)]
-- pairings :: [(FatesUnit, Maybe String)]
-- families :: [(FatesUnit, Just String)]
-- feed (map snd families) into lookup
-- produces [(FatesUnit, Maybe FatesUnit)]
-- we know it's a valid unit so we can use fromJust
-- to get [(FatesUnit, FatesUnit])


