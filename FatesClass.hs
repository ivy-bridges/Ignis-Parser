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

-- skill is (name of skill, level learned)
type Skill = (String, Int)


expectedStat :: Int -> Double -> Int -> Double
expectedStat base growth lvls = fromIntegral(base) + (growth)*fromIntegral(lvls)

--expectedStats :: BaseStats -> GrowthRates -> Int -> [Double]
--expectedStats bases growths lvls = map 


-- promotion set is another Class

-- class is bases + growths + skills + promotions
-- not worrying about weapon ranks or stuff like that
data FatesClass = FatesClass BaseStats --GrowthRates [Skill] [FatesClass]
  deriving Eq



-- okay. let's define. each of them. by hand. because
-- promoted and then basic so that we can actually list the promotions
-- except i don't think that matters in haskell, because it's Good

nohrPrince :: FatesClass
nohrPrince = FatesClass [17, 7, 3, 4, 5, 2, 5, 2]

nohrNoble :: FatesClass
nohrNoble = FatesClass [18, 8, 6, 4, 7, 2, 6, 6]

noshidoNoble :: FatesClass
noshidoNoble = FatesClass [19, 10, 4, 5, 6, 4, 7, 3]

-- nohrian classes

cavalier :: FatesClass
cavalier = FatesClass [17, 6, 0, 5, 5, 3, 5, 3]

paladin :: FatesClass
paladin = FatesClass [19, 8, 1, 7, 7, 4, 7, 6]

greatKnight :: FatesClass
greatKnight = FatesClass [21, 10, 0, 6, 6, 3, 10, 2]

knight :: FatesClass
knight = FatesClass [19, 8, 0, 5, 3, 3, 8, 1]

general :: FatesClass
general = FatesClass [22, 11, 0, 7, 3, 4, 12, 3]

fighter :: FatesClass
fighter = FatesClass [19, 7, 0, 6, 6, 2, 4, 1]

berserker :: FatesClass
berserker = FatesClass [24, 12, 0, 8, 9, 0, 5, 0]

mercenary :: FatesClass
mercenary = FatesClass [17, 5, 0, 7, 6, 2, 4, 2]

hero :: FatesClass
hero = FatesClass [20, 8, 0, 10, 8, 3, 7, 2]

bowKnight :: FatesClass
bowKnight = FatesClass [18, 6, 0, 8, 9, 3, 5, 6]

outlaw :: FatesClass
outlaw = FatesClass [16, 3, 1, 4, 8, 1, 2, 4]

adventurer :: FatesClass
adventurer = FatesClass [17, 4, 6, 6, 10, 2, 3, 8]

wyvernRider :: FatesClass
wyvernRider = FatesClass [17, 6, 0, 5, 4, 2, 7, 0]

wyvernLord :: FatesClass
wyvernLord = FatesClass [19, 8, 0, 9, 6, 3, 10, 1]

maligKnight :: FatesClass
maligKnight = FatesClass [18, 7, 6, 6, 5, 0, 8, 6]

darkMage :: FatesClass
darkMage = FatesClass [16, 0, 6, 3, 3, 1, 3, 5]

sorcerer :: FatesClass
sorcerer = FatesClass [17, 0, 9, 4, 6, 1, 5, 8]

darkKnight :: FatesClass
darkKnight = FatesClass [19, 8, 6, 6, 5, 3, 8, 6]

-- male and female troubadours get different skills
troubadourM :: FatesClass
troubadourM = FatesClass [15, 0, 3, 7, 5, 4, 1, 4]

troubadourF :: FatesClass
troubadourF = FatesClass [15, 0, 3, 7, 5, 4, 1, 4]

strategist :: FatesClass
strategist = FatesClass [16, 0, 7, 6, 7, 5, 2, 7]

-- maid and butler are statistically identical
maidButler :: FatesClass
maidButler = FatesClass [18, 4, 5, 9, 8, 4, 5, 4]

wolfskin :: FatesClass
wolfskin = FatesClass [19, 8, 0, 4, 6, 0, 4, 0]

wolfssegner :: FatesClass
wolfssegner = FatesClass [22, 11, 0, 6, 7, 1, 7, 1]

-- hoshidan classes

samurai :: FatesClass
samurai = FatesClass [17, 4, 0, 5, 8, 3, 3, 3]

swordmaster :: FatesClass
swordmaster = FatesClass [18, 6, 2, 7, 11, 4, 5, 5]

masterOfArms :: FatesClass
masterOfArms = FatesClass [20, 8, 0, 6, 9, 3, 7, 3]

oniSavage :: FatesClass
oniSavage = FatesClass [18, 6, 1, 2, 5, 0, 7, 1]

oniChieftain :: FatesClass
oniChieftain = FatesClass [19, 9, 5, 2, 7, 0, 10, 5]

blacksmith :: FatesClass
blacksmith = FatesClass [21, 8, 0, 9, 8, 3, 8, 2]

spearFighter :: FatesClass
spearFighter = FatesClass [17, 6, 0, 6, 6, 2, 5, 2]

spearMaster :: FatesClass
spearMaster = FatesClass [18, 9, 0, 8, 8, 3, 7, 3]

basara :: FatesClass
basara = FatesClass [20, 7, 5, 7, 7, 5, 7, 6]

diviner :: FatesClass
diviner = FatesClass [15, 0, 4, 5, 6, 1, 1, 3]

onmyoji :: FatesClass
onmyoji = FatesClass [16, 0, 7, 6, 7, 2, 3, 6]

-- monk and shrine maiden have different promotions

monk :: FatesClass
monk = FatesClass [16, 0, 3, 5, 5, 4, 2, 5]

shrineMaiden :: FatesClass
shrineMaiden = FatesClass [16, 0, 3, 5, 5, 4, 2, 5]

greatMaster :: FatesClass
greatMaster = FatesClass [19, 8, 6, 6, 8, 5, 6, 7]

priestess :: FatesClass
priestess = FatesClass [19, 6, 7, 6, 9, 5, 5, 8]

skyKnight :: FatesClass
skyKnight = FatesClass [16, 3, 0, 5, 7, 4, 2, 6]

falconKnight :: FatesClass
falconKnight = FatesClass [18, 5, 4, 6, 10, 5, 5, 9]

kinshiKnight :: FatesClass
kinshiKnight = FatesClass [17, 4, 1, 9, 8, 5, 4, 7]

archer :: FatesClass
archer = FatesClass [17, 5, 0, 7, 5, 2, 4, 1]

sniper :: FatesClass
sniper = FatesClass [19, 7, 0, 10, 9, 3, 6, 2]

ninja :: FatesClass
ninja = FatesClass [16, 3, 0, 8, 8, 1, 3, 3]

masterNinja :: FatesClass
masterNinja = FatesClass [17, 5, 0, 10, 11, 2, 4, 8]

mechanist :: FatesClass
mechanist = FatesClass [18, 7, 0, 9, 7, 2, 6, 6]

apothecary :: FatesClass
apothecary = FatesClass [18, 6, 0, 4, 4, 2, 6, 2]

merchant :: FatesClass
merchant = FatesClass [20, 8, 0, 6, 5, 4, 8, 5]

kitsune :: FatesClass
kitsune = FatesClass [16, 5, 1, 6, 8, 4, 1, 4]

nineTails :: FatesClass
nineTails = FatesClass [19, 6, 2, 9, 10, 5, 2, 8]

songstress :: FatesClass
songstress = FatesClass [16, 3, 0, 6, 5, 3, 2, 3]

villager :: FatesClass
villager = FatesClass [17, 5, 0, 4, 5, 3, 4, 0]

-- special class (TODO)


-- class inheritance for children goes in order of
-- [child's default] and then [father's class] and then [mother's class]
-- on each of the inheritance checks, it goes in order of
-- [primary class] and then [secondary class] and then parallels of each
-- azura cannot pass down songstress in the base game


-- need to do some testing to determine how the ordering works in the randomizer, since the order may differ



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

