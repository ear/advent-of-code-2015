{-# LANGUAGE TypeApplications, ViewPatterns #-}

import Text.Parsec
import Text.Parsec.String

import Data.Ord
import Data.List

import Control.Arrow

main = do
  Right op <- parseFromFile p "input.txt"
  print op
  print $ length inventories
  let ps = (playerFromInventory &&& id) <$> inventories
      winners = filter ((`beats` op) . fst) $ ps
  print . length $ winners
  let cheapest = minimumBy (comparing (cost . snd)) $ winners
  print cheapest
  print . cost . snd $ cheapest

cost
  (I (sum . map wcost -> ws) (sum . map acost -> as) (sum . map rcost -> rs)) =
    ws + as + rs

beats p1 op = p1 `kill` op <= op `kill` p1

kill (pdmg -> d) op = ceiling $
  fromIntegral (php op) / fromIntegral (d - parmor op)

data Player = P { php :: Int, pdmg :: Int, parmor :: Int }
  deriving (Show)

playerFromInventory i@(I (map wdmg -> ds) (map aarmor -> as) rs) =
  P { php = 100
    , pdmg = sum ds + sum (map rdmg rs)
    , parmor = sum as + sum (map rarmor rs) }

subsets [] = [[]]
subsets (x:xs) = let xss = subsets xs in xss ++ map (x:) xss

choose n = filter ((n ==) . length) . subsets

data Inventory = I [Weapon] [Armor] [Ring] deriving (Show)

inventories =
  [ I ws as rs
  | ws <- choose 1 weapons
  , as <- [0,1] >>= (`choose` armors)
  , rs <- [0,1,2] >>= (`choose` rings) ]

data Weapon = W { wname :: String, wcost :: Int, wdmg :: Int, warmor :: Int }
  deriving (Show)
data Armor  = A { aname :: String, acost :: Int, admg :: Int, aarmor :: Int }
  deriving (Show)
data Ring   = R { rname :: String, rcost :: Int, rdmg :: Int, rarmor :: Int }
  deriving (Show)

weapons =
  [ W "Dagger"       8 4 0
  , W "Shortsword"  10 5 0
  , W "Warhammer"   25 6 0
  , W "Longsword"   40 7 0
  , W "Greataxe"    74 8 0 ]
armors =
  [ A "Leather"     13 0 1
  , A "Chainmail"   31 0 2
  , A "Splintmail"  53 0 3
  , A "Bandedmail"  75 0 4
  , A "Platemail"  102 0 5 ]
rings =
  [ R "Damage +1"   25 1 0
  , R "Damage +2"   50 2 0
  , R "Damage +3"  100 3 0
  , R "Defense +1"  20 0 1
  , R "Defense +2"  40 0 2
  , R "Defense +3"  80 0 3 ]

p = do { string "Hit Points: "; hp <- number; newline
       ; string "Damage: "; d <- number; newline
       ; string "Armor: "; a <- number
       ; return P { php = hp, pdmg = d, parmor = a }
       }
  where
    number = read @Int <$> many1 digit
