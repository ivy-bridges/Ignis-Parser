module FatesUnit where

import Data.List

-- TODO: use actual datatypes

-- in the base game, these (non-corrin) characters have children
-- in a randomizer, the units who replace those characters are instead associated with their children
baseParents :: [String]
baseParents = ["Jakob", "Kaze", "Azura", "Silas", "Subaki", "Saizo", "Azama", "Hayato", "Hinata", "Takumi", "Kaden", "Ryoma", "Arthur", "Odin", "Niles", "Laslow", "Benny", "Leo", "Keaton", "Xander"]

baseChildren :: [String]
baseChildren = ["Dwyer", "Midori", "Shigure", "Sophie", "Caeldori", "Asugi", "Mitama", "Rhajat", "Hisame", "Kiragi", "Selkie", "Shiro", "Percy", "Ophelia", "Nina", "Soleil", "Ignatius", "Forrest", "Velouria", "Siegbert"]

basePairs = zip baseParents baseChildren

-- characters in the output log are blocks of text
-- given a block of text, determine whether the given character is a parent

isParent :: (String, String) -> Bool
isParent (a,b) = elem b baseParents


