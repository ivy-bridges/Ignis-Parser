import Data.List
import Control.Monad
import Data.Maybe
import Data.Tuple (swap)

import LogParser
import FatesUnit
import FatesClass (readClass)

-- currently asks for a log file and outputs a list of families
main :: IO ()
main = do
  putStrLn "Please enter the name of the output file."

  inputFile     <- getLine
  characterList <- loadReport inputFile

  let families = findFamilies characterList

  putStrLn "\nParent-Child pairings for this file..."
  forM_ families (\(parent, child) -> do
                     print parent
                     print child
                     putStrLn ""
                 )



-- loads a randomizer report
loadReport :: String -> IO [CharacterChunk]
loadReport inputFile = do
  logText <- readFile inputFile
  return $ toCharacters logText

-- constructs a FatesUnit based on info retrieved by LogParser
readChunk :: CharacterChunk -> FatesUnit
readChunk charChunk = readUnit name classes skills bases growths
  where
    name    = readName charChunk
    classes = (catMaybes . map readClass) (readClasses charChunk)  -- grab non-None classes
    skills  = readSkills charChunk
    bases   = readBases charChunk
    growths = readGrowths charChunk


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
findFamilies :: [CharacterChunk] -> [(FatesUnit, FatesUnit)]
findFamilies charList = unitFamilies
  where
    unitList      = map readChunk charList
    maybePairings = map (\unit -> (unit, findChildUnit charList unit)) unitList -- tries to find pairings for all units
    validPairings = filter (isJust . snd) maybePairings   -- where child actually exists
    unitFamilies  = map (\(parent, child) -> (parent, fromJust child)) validPairings

