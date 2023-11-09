module FatesUnit where

import Data.List

import FatesClass


-- (personal skill, [equipped skills])
type Skillset = (Skill, [Skill])

-- we care about a unit's name, class set, skill set, base stats, and growths
data FatesUnit = FatesUnit String [FatesClass] Skillset BaseStats GrowthRates
  deriving Eq

-- defining string representation
instance Show FatesUnit where
  show (FatesUnit name classes (personal, skills) bases growths) = unwords [name, classlist, personal]
    where classlist = show $ (map showClass classes)

-- accessor methods
getName :: FatesUnit -> String
getName (FatesUnit n _ _ _ _) = n

getClasses :: FatesUnit -> [FatesClass]
getClasses (FatesUnit _ c _ _ _) = c

getPrimary :: FatesUnit -> Skill
getPrimary (FatesUnit _ _ (p,_) _ _) = p

getEquipped :: FatesUnit -> [Skill]
getEquipped (FatesUnit _ _ (_,e) _ _) = e

getSkillset :: FatesUnit -> Skillset
getSkillset (FatesUnit _ _ s _ _) = s

getBases :: FatesUnit -> [Int]
getBases (FatesUnit _ _ _ b _) = b

getGrowths :: FatesUnit -> [Double]
getGrowths (FatesUnit _ _ _ _ g) = g


-- in the base game, these (non-corrin) characters have children
-- in a randomizer, the units who replace those characters are instead associated with their children
baseParents :: [String]
baseParents = ["Corrin (F)", "Corrin (M)", "Jakob", "Kaze", "Azura", "Silas", "Subaki", "Saizo", "Azama", "Hayato", "Hinata", "Takumi", "Kaden", "Ryoma", "Arthur", "Odin", "Niles", "Laslow", "Benny", "Leo", "Keaton", "Xander"]

baseChildren :: [String]
baseChildren = ["Kana (M)", "Kana (F)", "Dwyer", "Midori", "Shigure", "Sophie", "Caeldori", "Asugi", "Mitama", "Rhajat", "Hisame", "Kiragi", "Selkie", "Shiro", "Percy", "Ophelia", "Nina", "Soleil", "Ignatius", "Forrest", "Velouria", "Siegbert"]

-- associates each parent in the base game with their child unit
basePairs = zip baseParents baseChildren

-- for monk/shrine maiden and troubadour differences
maleUnits = ["Corrin (M)", "Jakob", "Kaze", "Silas", "Subaki", "Saizo", "Azama", "Hayato", "Hinata", "Takumi", "Kaden", "Ryoma", "Arthur", "Odin", "Niles", "Laslow", "Benny", "Leo", "Keaton", "Xander", "Izana", "Fuga", "Yukimura", "Kana (M)", "Dwyer", "Shigure", "Asugi", "Hisame", "Kiragi", "Shiro", "Percy", "Ignatius", "Forrest", "Siegbert", "Marth", "Ike", "Robin"]

-- units available on specific routes
birthrightUnits = ["Corrin (M)", "Corrin (F)", "Felicia", "Jakob", "Kaze", "Rinkah", "Azura", "Sakura", "Hana", "Subaki", "Silas", "Saizo", "Orochi", "Mozu", "Hinoka", "Azama", "Setsuna", "Hayato", "Oboro", "Hinata", "Takumi", "Kagero", "Reina", "Kaden", "Ryoma", "Scarlet", "Izana", "Shura", "Yukimura", "Kana (M)", "Kana (F)", "Shigure", "Dwyer", "Sophie", "Midori", "Shiro", "Kiragi", "Asugi", "Selkie", "Hisame", "Mitama", "Caeldori", "Rhajat", "Marth", "Ike", "Lucina", "Robin", "Anna"]

conquestUnits = ["Corrin (M)", "Corrin (F)", "Gunter", "Felicia", "Jakob", "Kaze", "Azura", "Elise", "Silas", "Arthur", "Effie", "Mozu", "Odin", "Niles", "Nyx", "Camilla", "Selena", "Beruka", "Laslow", "Peri", "Benny", "Charlotte", "Leo", "Keaton", "Xander", "Shura", "Flora", "Izana", "Kana (M)", "Kana (F)", "Shigure", "Dwyer", "Sophie", "Midori", "Siegbert", "Forrest", "Ignatius", "Velouria", "Percy", "Ophelia", "Soleil", "Nina", "Marth", "Ike", "Lucina", "Robin", "Anna"]

-- combine lists and remove exclusives
revelationUnits = nub $ (birthrightUnits ++ conquestUnits ++ ["Fuga"]) \\ ["Yukimura", "Izana"]

-- units available during the prologue
prologueUnits = ["Corrin (M)", "Corrin (F)", "Gunter", "Felicia", "Jakob", "Kaze", "Rinkah", "Azura", "Sakura"]

-- constructor that accounts for gendered class differences
readUnit :: String -> [FatesClass] -> Skillset -> BaseStats -> GrowthRates -> FatesUnit
readUnit name classes skills bases growths = FatesUnit name correctedClasses skills bases growths
  where
    correctedClasses = map (genderedClass (elem name maleUnits)) classes

-- return whether a character is a parent in the base game
isParent :: String -> Bool
isParent = flip elem baseParents


-- when passing classes down to a child in the base game, the father attempts before the mother
-- i'm not sure how that precedence works in a randomizer without further testing

-- collects passable clases from both, corrects for child's gender, then attempts passes in order of parent->spouse
childClassSet :: FatesUnit -> FatesUnit -> FatesUnit -> [FatesClass]
childClassSet parent spouse child = (determinePass spouseClasses . determinePass parentClasses) (getClasses child)
  where
    parentClasses = map (genderedClass childGender) $ (getInheritances . getClasses) parent
    spouseClasses = map (genderedClass childGender) $ (getInheritances . getClasses) spouse
    childGender   = elem (getName child) maleUnits




