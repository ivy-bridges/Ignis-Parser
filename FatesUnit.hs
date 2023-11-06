module FatesUnit where

import Data.List

import FatesClass

-- TODO: use actual datatypes

-- (personal skill, [equipped skills])
type Skillset = (Skill, [Skill])

-- fates unit need name, replaced, class, skills, base, growths
-- gender, parenthood, children can be stored in an external list

-- we care about a unit's name, classset, skillset, base stats, and growths
data FatesUnit = FatesUnit String [FatesClass] Skillset BaseStats GrowthRates



-- in the base game, these (non-corrin) characters have children
-- in a randomizer, the units who replace those characters are instead associated with their children
baseParents :: [String]
baseParents = ["Corrin (F)", "Corrin (M)", "Jakob", "Kaze", "Azura", "Silas", "Subaki", "Saizo", "Azama", "Hayato", "Hinata", "Takumi", "Kaden", "Ryoma", "Arthur", "Odin", "Niles", "Laslow", "Benny", "Leo", "Keaton", "Xander"]

baseChildren :: [String]
baseChildren = ["Kana (M)", "Kana (F)", "Dwyer", "Midori", "Shigure", "Sophie", "Caeldori", "Asugi", "Mitama", "Rhajat", "Hisame", "Kiragi", "Selkie", "Shiro", "Percy", "Ophelia", "Nina", "Soleil", "Ignatius", "Forrest", "Velouria", "Siegbert"]

-- associates each parent in the base game with their child unit
basePairs = zip baseParents baseChildren

-- for monk/shrine maiden and troubadour differences
-- i don't know a non-weird way to store the info required to distinguish
maleUnits = ["Corrin (M)", "Jakob", "Kaze", "Silas", "Subaki", "Saizo", "Azama", "Hayato", "Hinata", "Takumi", "Kaden", "Ryoma", "Arthur", "Odin", "Niles", "Laslow", "Benny", "Leo", "Keaton", "Xander", "Izana", "Fuga", "Yukimura", "Kana (M)", "Dwyer", "Shigure", "Asugi", "Hisame", "Kiragi", "Shiro", "Percy", "Ignatius", "Forrest", "Siegbert", "Marth", "Ike", "Robin"]



-- constructor that accounts for gendered class differences
readUnit :: String -> [FatesClass] -> Skillset -> BaseStats -> GrowthRates -> FatesUnit
readUnit name classes skills bases growths = FatesUnit name correctedClasses skills bases growths
  where
    correctedClasses = map (genderedClass (elem name maleUnits)) classes

-- characters in the output log are blocks of text
-- given a block of text, determine whether the given character is a parent

isParent :: (String, String) -> Bool
isParent (a,b) = elem b baseParents


