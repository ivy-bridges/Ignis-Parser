module FatesClass where

-- the ignis randomizer does not, to my knowledge, alter the behavior of any classes (skills, growths, promotions)
-- the purpose of this module is simply to format that data in a way that is understandable by FatesUnit

-- i don't think i'm going to do any skill-dipping pathfinding here
-- i do need to check how friendship and partner classes are passed, though



import Data.List
import Data.Tuple
import Data.Maybe

--i

-- growth rates are an ordered list of doubles
type GrowthRates = [Double]

-- base stats are an ordered list of ints 
type BaseStats = [Int]

-- class skills are stored as (name of skill, level learned)
type Skill = String


expectedStat :: Int -> Double -> Int -> Double
expectedStat base growth lvls = fromIntegral(base) + (growth/100)*fromIntegral(lvls)

--expectedStats :: BaseStats -> GrowthRates -> Int -> [Double]
--expectedStats bases growths lvls = map 


-- promotion set is another Class

-- class is bases + growths + skills + promotions
-- not worrying about weapon ranks or stuff like that
data FatesClass = FatesClass BaseStats GrowthRates [(Skill, Int)] [FatesClass]
  deriving (Eq, Show)



-- okay. let's define. each of them. by hand. because
-- promoted and then basic so that we can actually list the promotions
-- except i don't think that matters in haskell, because it's Good

nohrPrince :: FatesClass
nohrPrince = FatesClass
  [17, 7, 3, 4, 5, 2, 5, 2]
  [15, 15, 10, 10, 10, 10, 10, 5]
  [("Nobility", 1), ("Dragon Fang", 10)]
  [nohrNoble, hoshidoNoble]

nohrNoble :: FatesClass
nohrNoble = FatesClass
  [18, 8, 6, 4, 7, 2, 6, 6]
  [15, 10, 15, 5, 15, 5, 5, 15]
  [("Draconic Hex", 5), ("Nohrian Trust", 15)]
  []

hoshidoNoble :: FatesClass
hoshidoNoble = FatesClass
  [19, 10, 4, 5, 6, 4, 7, 3]
  [15, 15, 10, 10, 10, 10, 15, 0]
  [("Dragon Ward", 5), ("Hoshidan Unity", 15)]
  []

-- nohrian classes

cavalier :: FatesClass
cavalier = FatesClass
  [17, 6, 0, 5, 5, 3, 5, 3]
  [10, 15, 0, 10, 10, 15, 10, 5]
  [("Elbow Room", 1), ("Shelter", 10)]
  [paladin, greatKnight]

paladin :: FatesClass
paladin = FatesClass
  [19, 8, 1, 7, 7, 4, 7, 6]
  [10, 15, 0, 10, 10, 15, 10, 10]
  [("Defender", 5), ("Aegis", 15)]
  []

greatKnight :: FatesClass
greatKnight = FatesClass
  [21, 10, 0, 6, 6, 3, 10, 2]
  [20, 20, 0, 10, 5, 5, 20, 0]
  [("Luna", 5), ("Armored Blow", 15)]
  []

knight :: FatesClass
knight = FatesClass
  [19, 8, 0, 5, 3, 3, 8, 1]
  [20, 20, 0, 15, 5, 10, 20, 0]
  [("Defense +2", 1), ("Natural Cover", 10)]
  [greatKnight, general]

general :: FatesClass
general = FatesClass
  [22, 11, 0, 7, 3, 4, 12, 3]
  [25, 20, 0, 15, 0, 10, 20, 5]
  [("Wary Fighter", 5), ("Pavise", 15)]
  []

fighter :: FatesClass
fighter = FatesClass
  [19, 7, 0, 6, 6, 2, 4, 1]
  [20, 20, 0, 15, 15, 5, 5, 0]
  [("HP +5", 1),("Gamble", 10)]
  [berserker, hero]

berserker :: FatesClass
berserker = FatesClass
  [24, 12, 0, 8, 9, 0, 5, 0]
  [30, 25, 0, 15, 15, 0, 0, 0]
  [("Rally Strength", 5), ("Axefaire", 15)]
  []

mercenary :: FatesClass
mercenary = FatesClass
  [17, 5, 0, 7, 6, 2, 4, 2]
  [20, 15, 0, 20, 15, 5, 10, 0]
  [("Good Fortune", 1), ("Strong Riposte", 10)]
  [hero, bowKnight]

hero :: FatesClass
hero = FatesClass
  [20, 8, 0, 10, 8, 3, 7, 2]
  [20, 15, 0, 20, 15, 5, 10, 0]
  [("Sol", 5), ("Axebreaker", 15)]
  []

bowKnight :: FatesClass
bowKnight = FatesClass
  [18, 6, 0, 8, 9, 3, 5, 6]
  [10, 10, 0, 15, 15, 10, 0, 10]
  [("Rally Skill", 5), ("Shurikenbreaker", 15)]
  []

outlaw :: FatesClass
outlaw = FatesClass
  [16, 3, 1, 4, 8, 1, 2, 4]
  [0, 10, 5, 10, 20, 0, 0, 20]
  [("Locktouch", 1), ("Movement +1", 10)]
  [bowKnight, adventurer]

adventurer :: FatesClass
adventurer = FatesClass
  [17, 4, 6, 6, 10, 2, 3, 8]
  [0, 5, 15, 5, 20, 0, 0, 20]
  [("Lucky Seven", 5), ("Pass", 15)]
  []

wyvernRider :: FatesClass
wyvernRider = FatesClass
  [17, 6, 0, 5, 4, 2, 7, 0]
  [10, 15, 0, 15, 10, 5, 20, 0]
  [("Strength +2", 1), ("Lunge", 10)]
  [wyvernLord, maligKnight]

wyvernLord :: FatesClass
wyvernLord = FatesClass
  [19, 8, 0, 9, 6, 3, 10, 1]
  [10, 15, 0, 15, 10, 5, 20, 0]
  [("Rally Defense", 5), ("Swordbreaker", 15)]
  []

maligKnight :: FatesClass
maligKnight = FatesClass
  [18, 7, 6, 6, 5, 0, 8, 6]
  [0, 15, 15, 10, 5, 0, 10, 15]
  [("Savage Blow", 5), ("Trample", 15)]
  []

darkMage :: FatesClass
darkMage = FatesClass
  [16, 0, 6, 3, 3, 1, 3, 5]
  [0, 10, 20, 0, 10, 0, 5, 10]
  [("Heartseeker", 1), ("Malefic Aura", 10)]
  [sorcerer, darkKnight]

sorcerer :: FatesClass
sorcerer = FatesClass
  [17, 0, 9, 4, 6, 1, 5, 8]
  [0, 0, 25, 0, 10, 0, 5, 15]
  [("Vengeance", 5), ("Bowbreaker", 15)]
  []

darkKnight :: FatesClass
darkKnight = FatesClass
  [19, 8, 6, 6, 5, 3, 8, 6]
  [15, 20, 10, 5, 5, 5, 15, 5]
  [("Seal Magic", 5), ("Lifetaker", 15)]
  []

-- male and female troubadours get different skills
troubadourM :: FatesClass
troubadourM = FatesClass
  [15, 0, 3, 7, 5, 4, 1, 4]
  [0, 0, 10, 20, 10, 15, 0, 15]
  [("Resistance +2", 1), ("Gentilhomme", 10)]
  [strategist, maidButler]

troubadourF :: FatesClass
troubadourF = FatesClass
  [15, 0, 3, 7, 5, 4, 1, 4]
  [0, 0, 10, 20, 10, 15, 0, 15]
  [("Resistance +2", 1), ("Demoiselle", 10)]
  [strategist, maidButler]

strategist :: FatesClass
strategist = FatesClass
  [16, 0, 7, 6, 7, 5, 2, 7]
  [0, 0, 15, 5, 10, 20, 0, 15]
  [("Rally Resistance", 5), ("Inspiration", 15)]
  []

-- maid and butler are statistically identical
maidButler :: FatesClass
maidButler = FatesClass
  [18, 4, 5, 9, 8, 4, 5, 4]
  [0, 10, 10, 15, 15, 10, 5, 10]
  [("Live To Serve", 5), ("Tomebreaker", 15)]
  []

wolfskin :: FatesClass
wolfskin = FatesClass
  [19, 8, 0, 4, 6, 0, 4, 0]
  [20, 20, 0, 5, 15, 5, 10, 0]
  [("Odd Shaped", 1), ("Beastbane", 10)]
  [wolfssegner]

wolfssegner :: FatesClass
wolfssegner = FatesClass
  [22, 11, 0, 6, 7, 1, 7, 1]
  [20, 20, 0, 5, 15, 5, 10, 0]
  [("Better Odds", 5), ("Grisly Wound", 15)]
  []
-- hoshidan classes

samurai :: FatesClass
samurai = FatesClass
  [17, 4, 0, 5, 8, 3, 3, 3]
  [10, 10, 0, 15, 20, 15, 0, 10]
  [("Duelist's Blow", 1), ("Vantage", 10)]
  [swordmaster, masterOfArms]

swordmaster :: FatesClass
swordmaster = FatesClass
  [18, 6, 2, 7, 11, 4, 5, 5]
  [10, 10, 5, 15, 20, 15, 0, 10]
  [("Astra", 5), ("Swordfaire", 15)]
  []

masterOfArms :: FatesClass
masterOfArms = FatesClass
  [20, 8, 0, 6, 9, 3, 7, 3]
  [20, 15, 0, 10, 10, 10, 10, 0]
  [("Seal Strength", 5), ("Life and Death", 15)]
  []

oniSavage :: FatesClass
oniSavage = FatesClass
  [18, 6, 1, 2, 5, 0, 7, 1]
  [20, 20, 10, 0, 10, 0, 20, 0]
  [("Seal Resistance", 1), ("Shove", 10)]
  [oniChieftain, blacksmith]

oniChieftain :: FatesClass
oniChieftain = FatesClass
  [19, 9, 5, 2, 7, 0, 10, 5]
  [10, 20, 15, 0, 10, 0, 20, 5]
  [("Death Blow", 5), ("Counter", 15)]
  []

blacksmith :: FatesClass
blacksmith = FatesClass
  [21, 8, 0, 9, 8, 3, 8, 2]
  [20, 15, 0, 15, 10, 5, 15, 0]
  [("Salvage Blow", 5), ("Lancebreaker", 15)]
  []

spearFighter :: FatesClass
spearFighter = FatesClass
  [17, 6, 0, 6, 6, 2, 5, 2]
  [15, 15, 0, 15, 15, 5, 10, 5]
  [("Seal Defense", 5), ("Swap", 10)]
  [spearMaster, basara]

spearMaster :: FatesClass
spearMaster = FatesClass
  [18, 9, 0, 8, 8, 3, 7, 3]
  [15, 15, 0, 15, 15, 5, 10, 5]
  [("Seal Speed", 5), ("Lancefaire", 15)]
  []

basara :: FatesClass
basara = FatesClass
  [20, 7, 5, 7, 7, 5, 7, 6]
  [20, 10, 10, 10, 10, 15, 5, 10]
  [("Rend Heaven", 5), ("Quixotic", 15)]
  []

diviner :: FatesClass
diviner = FatesClass
  [15, 0, 4, 5, 6, 1, 1, 3]
  [0, 5, 15, 10, 15, 5, 0, 10]
  [("Magic +2", 1), ("Future Sight", 10)]
  [basara, onmyoji]

onmyoji :: FatesClass
onmyoji = FatesClass
  [16, 0, 7, 6, 7, 2, 3, 6]
  [0, 0, 20, 10, 15, 0, 0, 15]
  [("Rally Magic", 5), ("Tomefaire", 15)]
  []

-- monk and shrine maiden have different promotions

monk :: FatesClass
monk = FatesClass
  [16, 0, 3, 5, 5, 4, 2, 5]
  [0, 5, 10, 10, 15, 15, 0, 20]
  [("Miracle", 1), ("Rally Luck", 10)]
  [onmyoji, greatMaster]

shrineMaiden :: FatesClass
shrineMaiden = FatesClass
  [16, 0, 3, 5, 5, 4, 2, 5]
  [0, 5, 10, 10, 15, 15, 0, 20]
  [("Miracle", 1), ("Rally Luck", 10)]
  [onmyoji, priestess]

greatMaster :: FatesClass
greatMaster = FatesClass
  [19, 8, 6, 6, 8, 5, 6, 7]
  [10, 15, 5, 5, 15, 15, 10, 10]
  [("Renewal", 5), ("Countermagic", 15)]
  []

priestess :: FatesClass
priestess = FatesClass
  [19, 6, 7, 6, 9, 5, 5, 8]
  [10, 10, 10, 5, 15, 15, 0, 20]
  [("Renewal", 5), ("Countermagic", 15)]
  []

skyKnight :: FatesClass
skyKnight = FatesClass
  [16, 3, 0, 5, 7, 4, 2, 6]
  [0, 10, 0, 10, 15, 20, 0, 20]
  [("Darting Blow", 1), ("Camaraderie", 10)]
  [falconKnight, kinshiKnight]

falconKnight :: FatesClass
falconKnight = FatesClass
  [18, 5, 4, 6, 10, 5, 5, 9]
  [0, 10, 10, 10, 15, 20, 0, 20]
  [("Rally Speed", 5), ("Warding Blow", 15)]
  []

kinshiKnight :: FatesClass
kinshiKnight = FatesClass
  [17, 4, 1, 9, 8, 5, 4, 7]
  [0, 5, 0, 15, 15, 15, 0, 15]
  [("Air Superiority", 5), ("Amaterasu", 15)]
  []

archer :: FatesClass
archer = FatesClass
  [17, 5, 0, 7, 5, 2, 4, 1]
  [10, 15, 0, 15, 15, 5, 10, 0]
  [("Skill +2", 1), ("Quick Draw", 10)]
  [kinshiKnight, sniper]

sniper :: FatesClass
sniper = FatesClass
  [19, 7, 0, 10, 9, 3, 6, 2]
  [10, 15, 0, 20, 15, 5, 10, 0]
  [("Certain Blow", 5), ("Bowfaire", 15)]
  []

ninja :: FatesClass
ninja = FatesClass
  [16, 3, 0, 8, 8, 1, 3, 3]
  [5, 5, 0, 20, 20, 0, 5, 15]
  [("Locktouch", 1), ("Poison Strike", 10)]
  [masterNinja, mechanist]

masterNinja :: FatesClass
masterNinja = FatesClass
  [17, 5, 0, 10, 11, 2, 4, 8]
  [5, 5, 0, 20, 20, 0, 5, 20]
  [("Lethality", 5), ("Shurikenfaire", 15)]
  []

mechanist :: FatesClass
mechanist = FatesClass
  [18, 7, 0, 9, 7, 2, 6, 6]
  [10, 10, 0, 15, 10, 5, 5, 15]
  [("Golembane", 5), ("Replicate", 15)]
  []

apothecary :: FatesClass
apothecary = FatesClass
  [18, 6, 0, 4, 4, 2, 6, 2]
  [20, 20, 0, 10, 10, 5, 10, 5]
  [("Potent Potion", 1), ("Quick Salve", 10)]
  [mechanist, merchant]

merchant :: FatesClass
merchant = FatesClass
  [20, 8, 0, 6, 5, 4, 8, 5]
  [20, 20, 0, 10, 5, 15, 10, 5]
  [("Profiteer", 5), ("Spendthrift", 15)]
  []

kitsune :: FatesClass
kitsune = FatesClass
  [16, 5, 1, 6, 8, 4, 1, 4]
  [10, 10, 0, 15, 20, 10, 0, 20]
  [("Evenhanded", 1), ("Beastbane", 10)]
  [nineTails]

nineTails :: FatesClass
nineTails = FatesClass
  [19, 6, 2, 9, 10, 5, 2, 8]
  [10, 10, 0, 15, 20, 10, 0, 20]
  [("Even Better", 5), ("Grisly Wound", 15)]
  []

songstress :: FatesClass
songstress = FatesClass
  [16, 3, 0, 6, 5, 3, 2, 3]
  [0, 10, 0, 20, 20, 20, 0, 0]
  [("Luck +4", 1), ("Inspiring Song", 10), ("Voice of Peace", 25), ("Foreign Princess", 35)]
  []

villager :: FatesClass
villager = FatesClass
  [17, 5, 0, 4, 5, 3, 4, 0]
  [10, 10, 0, 10, 10, 20, 10, 0]
  [("Aptitude", 1), ("Underdog", 10)]
  [masterOfArms, merchant]

-- dlc and amiibo classes

dreadFighter :: FatesClass
dreadFighter = FatesClass
  [19, 8, 3, 6, 8, 1, 6, 9]
  [15, 15, 5, 5, 15, 0, 5, 20]
  [("Even Keel", 1), ("Iron Will", 10), ("Clarity", 25), ("Aggressor", 35)]
  []

darkFalcon :: FatesClass
darkFalcon = FatesClass
  [17, 4, 7, 5, 9, 4, 3, 9]
  [0, 10, 15, 5, 15, 15, 0, 20]
  [("Speed +2", 1), ("Relief", 10), ("Rally Movement", 25), ("Galeforce", 35)]
  []

ballistician :: FatesClass
ballistician = FatesClass
  [18, 10, 0, 7, 2, 4, 3, 1]
  [5, 25, 0, 15, 0, 10, 5, 5]
  [("Survey", 1), ("Opportunity Shot", 10), ("Rifled Barrel", 25), ("Surefooted", 35)]
  []

witch :: FatesClass
witch = FatesClass
  [17, 0, 10, 5, 9, 3, 4, 5]
  [5, 0, 25, 5, 20, 5, 0, 10]
  [("Shadowgift", 1), ("Witch's Brew", 10), ("Warp", 25), ("Toxic Brew", 35)]
  []

lodestar :: FatesClass
lodestar = FatesClass
  [19, 7, 0, 10, 9, 7, 7, 2]
  [15, 10, 0, 20, 10, 25, 5, 5]
  [("Dancing Blade", 1), ("Charm", 10), ("Dual Guarder", 25), ("Speedtaker", 35)]
  []

vanguard :: FatesClass
vanguard = FatesClass
  [21, 10, 0, 6, 7, 3, 9, 1]
  [20, 20, 0, 5, 5, 10, 15, 0]
  [("Heavy Blade", 1), ("Veteran Intuition", 10), ("Aether", 25), ("Strengthtaker", 35)]
  []

greatLord :: FatesClass
greatLord = FatesClass
  [18, 8, 1, 8, 9, 5, 7, 3]
  [15, 15, 0, 15, 10, 15, 10, 5]
  [("Dual Striker", 1), ("Charm", 10), ("Aether", 25), ("Awakening", 35)]
  []

grandmaster :: FatesClass
grandmaster = FatesClass
  [18, 7, 6, 8, 7, 2, 6, 8]
  [10, 15, 15, 15, 5, 0, 5, 15]
  [("Tactical Advice", 1), ("Solidarity", 10), ("Ignis", 25), ("Rally Spectrum", 35)]
  []


-- class inheritance for children goes in order of
-- [child's default] and then [father's class] and then [mother's class]
-- on each of the inheritance checks, it goes in order of
-- [primary class] and then [secondary class] and then parallels of each
-- azura cannot pass down songstress in the base game


-- class names used in the log file
classNames :: [(String, FatesClass)]
classNames =
  [("Nohr Prince", nohrPrince),
   ("Nohr Princess", nohrPrince),
   ("Cavalier", cavalier),
   ("Paladin", paladin),
   ("Great Knight", greatKnight),
   ("Knight", knight),
   ("General", general),
   ("Fighter", fighter),
   ("Berserker", berserker),
   ("Mercenary", mercenary),
   ("Hero", hero),
   ("Bow Knight", bowKnight),
   ("Outlaw", outlaw),
   ("Wyvern Rider", wyvernRider),
   ("Wyvern Lord", wyvernLord),
   ("Malig Knight", maligKnight),
   ("Dark Mage", darkMage),
   ("Sorcerer", sorcerer),
   ("Dark Knight", darkKnight),
   ("Troubadour", troubadourM), -- can only be resolved by knowing unit
   ("Strategist", strategist),
   ("Maid", maidButler),
   ("Butler", maidButler),
   ("Wolfskin", wolfskin),
   ("Wolfssegner", wolfssegner),
   ("Samurai", samurai),
   ("Swordmaster", swordmaster),
   ("Master of Arms", masterOfArms),
   ("Oni Savage", oniSavage),
   ("Oni Chieftain", oniChieftain),
   ("Blacksmith", blacksmith),
   ("Spear Fighter", spearFighter),
   ("Basara", basara),
   ("Diviner", diviner),
   ("Onmyoji", onmyoji),
   ("Monk", monk),
   ("Shrine Maiden", shrineMaiden),
   ("Great Master", greatMaster),
   ("Priestess", priestess),
   ("Sky Knight", skyKnight),
   ("Falcon Knight", falconKnight),
   ("Kinshi Knight", kinshiKnight),
   ("Archer", archer),
   ("Sniper", sniper),
   ("Ninja", ninja),
   ("Master Ninja", masterNinja),
   ("Mechanist", mechanist),
   ("Apothecary", apothecary),
   ("Merchant", merchant),
   ("Kitsune", kitsune),
   ("Nine-Tails", nineTails),
   ("Songstress", songstress),
   ("Villager", villager),
   ("Dread Fighter", dreadFighter),
   ("Dark Falcon", darkFalcon),
   ("Ballistician", ballistician),
   ("Witch", witch),
   ("Lodestar", lodestar),
   ("Vanguard", vanguard),
   ("Great Lord", greatLord),
   ("Grandmaster", grandmaster)]


-- attempt to read a class from a string
readClass :: String -> Maybe FatesClass
readClass = (flip lookup) classNames




-- these are the parallel classes used when a child already has both primary and secondary
-- parallel classes are NOT symmetrical (!!), and some classes have none
-- sourced from selenes forest
-- https://serenesforest.net/fire-emblem-fates/classes/parallel-classes/
parallelClasses :: [(FatesClass, FatesClass)]
parallelClasses = [(cavalier,    ninja),
                   (knight,      spearFighter),
                   (fighter,     oniSavage),
                   (mercenary,   samurai),
                   (outlaw,      archer),
                   (samurai,     mercenary), 
                   (diviner,     darkMage),
                   (wyvernRider, skyKnight),
                   (ninja,       cavalier),
                   (darkMage,    diviner),
                   (wolfskin,    outlaw),
                   (kitsune,     apothecary),
                   (songstress,  troubadourM),
                   (villager,    apothecary)]


-- get a parallel class if one exists
getParallel :: FatesClass -> Maybe FatesClass
getParallel originalClass = snd <$> potentialMatch
  where
    potentialMatch = find (\(a,b) -> a == originalClass) parallelClasses
    -- could use concatenation instead of this lambda but that's uglier

-- correct for gendered classes, if necessary
-- here, "True" indicates that a unit is male
genderedClass :: Bool -> FatesClass -> FatesClass
genderedClass True c
  | c == shrineMaiden        = monk
  | c == troubadourF         = troubadourM
genderedClass False c
  | c == monk                = shrineMaiden
  | c == troubadourM         = troubadourF
genderedClass _ agenderClass = agenderClass


-- given a list of classes, get the list of classes that the unit will attempt to pass down for inheritance
-- this just grabs the parallel classes for each, if any, in order
getInheritances :: [FatesClass] -> [FatesClass]
getInheritances classes = classes ++ parallels
  where parallels = catMaybes (map getParallel classes)


-- given a list of classes from a to b, determine which class from a gets passed to b
-- primary, secondaries, and parallels should be put together and ordered before calling this function
determinePass :: [FatesClass] -> [FatesClass] -> Maybe FatesClass
determinePass [] _ = Nothing
determinePass source dest
  | passAttempt `elem` dest = determinePass (tail source) dest
  | otherwise = Just passAttempt
  where passAttempt = head source

