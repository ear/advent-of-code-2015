{-# LANGUAGE TypeApplications, ViewPatterns #-}

import Text.Parsec
import Text.Parsec.String

import Data.Ord
import Data.List

import Control.Arrow

main = do
  Right op <- parseFromFile p "input.txt"
  print op

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
  [ S "Magic Missile"  53 $ E 0   0 0 4 0
  , S "Drain"          73 $ E 0   0 2 2 0
  , S "Shield"        113 $ E 6   0 0 3 0
  , S "Recharge"      229 $ E 5 101 0 0 0 ]

p = do { string "Hit Points: "; hp <- number; newline
       ; string "Damage: "; d <- number
       ; return P { php = hp, pmana = 0, pdmg = d, parmor = 0 }
       }
  where
    number = read @Int <$> many1 digit
