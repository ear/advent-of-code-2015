{-# LANGUAGE TypeApplications, ViewPatterns #-}

import Text.Parsec
import Text.Parsec.String

import Data.Ord
import Data.List

import Control.Arrow

main = do
  Right op <- parseFromFile p "input.txt"
  print op
  -- magic missile-only strat
  putStrLn "=== Magic Missile-only strategy ==="
  putStr "boss kills player in #turns: "
  print $ php player `divMod` pdmg op
  putStr "player kills boss in #turns: "
  print $ php op `divMod` edmg (seffect $ spells !! 0)
  -- shield-once, then magic missile
  putStrLn "=== Shield-once, then Magic Missile strategy ==="
  let hp = php player
      dmg = pdmg op
      effs = 0 : replicate 6 7 ++ repeat 0
      dmgs = cycle [0,dmg]
      herolife = scanl (\h (e,d) -> h + e - d) hp $ zip effs dmgs
      heroalivespan = span (>=0) herolife
  putStr "boss kills player in #turns: "
  print . length . fst $ heroalivespan
  putStr "player kills boss in #turns: "
  print $ (succ *** id) $ php op `divMod` edmg (seffect $ spells !! 0)
  putStrLn "only problem, we run out of mana, so have to Recharge too (:"

data Player = P
  { php :: Int, pmana :: Int, pdmg :: Int, parmor :: Int }
  deriving Show

player = P { php = 50, pmana = 100, pdmg = 0, parmor = 0 }

data Spell = S
  { sname :: String, scost :: Int, seffect :: Effect }
  deriving Show

data Effect = E
  { eturns :: Int, emana :: Int, ehealth :: Int, edmg :: Int, earmor :: Int }
  deriving Show

spells =
  [ S "Magic Missile"  53 $
    E { eturns = 0, emana =   0, ehealth = 0, edmg = 4, earmor = 0 }
  , S "Drain"          73 $
    E { eturns = 0, emana =   0, ehealth = 2, edmg = 2, earmor = 0 }
  , S "Shield"        113 $
    E { eturns = 6, emana =   0, ehealth = 0, edmg = 0, earmor = 7 }
  , S "Poison"        173 $
    E { eturns = 6, emana =   0, ehealth = 3, edmg = 0, earmor = 0 }
  , S "Recharge"      229 $
    E { eturns = 5, emana = 101, ehealth = 0, edmg = 0, earmor = 0 } ]

p = do { string "Hit Points: "; hp <- number; newline
       ; string "Damage: "; d <- number
       ; return P { php = hp, pmana = 0, pdmg = d, parmor = 0 }
       }
  where
    number = read @Int <$> many1 digit
