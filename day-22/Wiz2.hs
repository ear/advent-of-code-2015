{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

import Data.List ( intersperse, foldl', sortBy )
import Data.Maybe ( catMaybes )
import Data.Monoid ( Sum(..) )
import Data.Ord ( comparing )

import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Map ( Map )
import qualified Data.Map.Strict as Map

import System.Environment ( getArgs )

main = mapM_ print
     $ sortBy (comparing fst)
     $ map (\s -> (spellCost (pSpells s), s))
     $ filter (\State{..} -> pHp > 0 && bHp < 1)
     $ turn 18

data Spell
  = M -- Magic Missle
  | D -- Drain
  | S -- Shield
  | P -- Poison
  | R -- Recharge
  deriving (Ord, Eq, Show)

spells :: Set Spell
spells = Set.fromList [ M, D, S, P, R ]

costs :: Map Spell Int
costs = Map.fromList $ zip (Set.toList spells) [ 53, 73, 113, 173, 229 ]

cost :: Spell -> Int
cost = (costs Map.!)

data State
  = State
    { pHp     :: Int              -- ^ player hit points
    , pMana   :: Int              -- ^ available mana

    , pArmor  :: Int              -- ^ player's armor

    , bHp     :: Int              -- ^ boss hit points
    , bDmg    :: Int              -- ^ boss hit damage

    , pSpent  :: Int              -- ^ mana spent

    , pAvail  :: Set Spell        -- ^ available effects
    , pActive :: Map Spell Int    -- ^ active effects

    , pSpells :: [Spell]          -- ^ spell history thus far
    }
  deriving (Show)

mkS php pmana bhp bdmg = State
  { pHp   = php
  , pMana = pmana

  , pArmor = 0

  , bHp  = bhp
  , bDmg = bdmg

  , pSpent = 0

  , pAvail  = spells
  , pActive = Map.empty

  , pSpells = []
  }

pp :: State -> [Spell]
pp State{pSpells} = reverse pSpells

hardMode :: State -> State
hardMode s@State{..} = s { pHp = pHp - 1 }

-- | given a state compute the boss turn
boss :: State -> State
boss (tickEffects -> s@State{..}) = s { pHp = pHp - dmg }
  where
    dmg | bHp <= 0  = 0
        | otherwise = max 1 (bDmg - pArmor)

-- | given a state compute the possible spells to cast and their respective states
nexts :: State -> [State]
nexts (tickEffects -> state) = do
  spell <- Set.toList $ available state
  return $ cast spell state

tickEffects :: State -> State
tickEffects s@State{..} = s2 { pAvail = Set.union pAvail done
                             , pActive = remain }
  where
    s1 = foldl' (flip performEffect) s $ Map.keys pActive
    (Map.keysSet -> done, remain) = Map.partition (<=0) . Map.map pred $ pActive
    s2 = foldl' (flip undoEffect) s1 $ Set.toList done


available :: State -> Set Spell
available State{..} = affordable
  where
    unused = spells Set.\\ Map.keysSet pActive
    affordable = Map.keysSet $ Map.filter (<= pMana) $ costs `Map.restrictKeys` unused

cast spell (hardMode -> s0@State{..})
  | hasEffect spell = s1 { pAvail  = Set.delete spell pAvail
                         , pActive = Map.insert spell (duration spell) pActive }
  | otherwise = performSpell spell s1
  where
    s1 = s0 { pMana = pMana - cost spell
            , pSpells = spell : pSpells }

hasEffect :: Spell -> Bool
hasEffect S = True
hasEffect P = True
hasEffect R = True
hasEffect _ = False

duration :: Spell -> Int
duration S = 6
duration P = 6
duration R = 5
duration _ = error "asking duration for spell with no lasting effect"

performSpell :: Spell -> State -> State
performSpell M = \s@State{..} -> s { bHp = bHp - 4                }
performSpell D = \s@State{..} -> s { bHp = bHp - 2, pHp = pHp + 2 }
performSpell _ = error "performSpell on an effect"

performEffect :: Spell -> State -> State
performEffect S = \s@State{..} -> s { pArmor = 7           }
performEffect P = \s@State{..} -> s { bHp    = bHp - 3     }
performEffect R = \s@State{..} -> s { pMana  = pMana + 101 }
performEffect _ = error "performEffect on a Spell"

undoEffect :: Spell -> State -> State
undoEffect S = \s@State{..} -> s { pArmor = 0 }
undoEffect _ = id

spellCost :: [Spell] -> Int
spellCost = getSum . foldMap (Sum . cost)

turn 1 = nexts (mkS 50 500 58 9)
turn n | even n = boss <$> turn (n-1)
       | odd n = turn (n-1) >>= nexts
