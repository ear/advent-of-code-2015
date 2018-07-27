{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

import Data.List ( intersperse )
import Data.Maybe ( catMaybes )

import System.Environment ( getArgs )

-- main = mapM_ print
--      . filter (\s -> case s of Won _ -> True; _ -> False)
--      . catMaybes
--      . map play
--      $ allPlays

-- main = mapM_ print
--      . filter (\(s,_) -> case s of Won _ -> True; _ -> False)
--      . catMaybes
--      . map playAndLog
--      $ allPlays

main = do
  [n] <- map (read @Int) <$> getArgs
  mapM_ print
    . filter (\(s,_) -> case s of Won _ -> True; _ -> False)
    . catMaybes
    . map playAndLog
    $ boundedPlays n

alphabet = "MDSPR"

allPlays :: [String]
allPlays = map pure alphabet ++ do
  play <- allPlays
  c <- alphabet
  return $ c:play

boundedPlays :: Int -> [String]
boundedPlays 0 = []
boundedPlays n = map pure alphabet ++ do
  play <- boundedPlays (n-1)
  c <- alphabet
  return $ c:play


data Spell = Spell
  { spCost :: Int, spDmg :: Int, spHeal :: Int, spEffect :: Maybe Effect }
  deriving Show

data Effect = Effect
  { eName :: Char, eDuration :: Int, eArmor :: Int, eDmg :: Int, eMana :: Int }
  deriving Show

spell :: Char -> Spell
spell 'M' = Spell  53 4 0 Nothing
spell 'D' = Spell  73 2 2 Nothing
spell 'S' = Spell 113 0 0 $ Just $ Effect 'S' 6 7 0   0
spell 'P' = Spell 173 0 0 $ Just $ Effect 'P' 6 0 3   0
spell 'R' = Spell 229 0 0 $ Just $ Effect 'R' 5 0 0 101

data State
  = State { pHp    :: Int      -- ^ player hit points
          , pArmor :: Int      -- ^ player armor
          , pMana  :: Int      -- ^ player mana
          , pSpent :: Int      -- ^ player mana spent so far
          , bHp    :: Int      -- ^ boss hit points
          , bDmg   :: Int      -- ^ boss damage
          , effs   :: [Effect] -- ^ active effects
          }
  | Won   { pSpent :: Int }
  | Lost
  deriving Show

effect :: Effect -> State -> ([Effect],State)
effect e@Effect{..} s@State{..} = (e', s')
  where
    e' | eDuration == 0 = mempty
       | otherwise      = pure $ e { eDuration = eDuration - 1 }
    s' = s { -- pArmor = eArmor
             bHp    = bHp    - eDmg
           , pMana  = pMana  + eMana
           }

step :: Maybe Spell -- ^ Nothing -> Boss turn; Just spell -> Player turn
     -> State       -- ^ current state
     -> Maybe State -- ^ next state or error
step Nothing s
  | bHp <= 0 = Just $! Won pSpent
  | pNewHp <= 0 = Nothing -- Just Lost -- XXX Nothing?
  | otherwise   = Just s'{ pHp = pNewHp }
  where s'@State{..} = applyEffects s
        armor = maximum (pArmor : map eArmor effs)
        pNewHp | bDmg - armor < 1 = pHp - 1
               | otherwise        = pHp - (bDmg - armor)
step (Just Spell{..}) s
  = let s' = applyEffects s
        s'' = s' { pHp = (pHp s') + spHeal
                 , pMana = (pMana s') - spCost
                 , pSpent = (pSpent s') + spCost
                 , bHp = (bHp s') - spDmg
                 , effs = effs' }
        effs' | Just e <- spEffect = effs s' ++ [e]
              | otherwise          = effs s'
    in if spCost > pMana s' || effectAlreadyActive s spEffect
       then Nothing
       else if bHp s'' <= 0
            then Just $! Won (pSpent s'')
            else Just $! s''

-- step :: Maybe Spell -- ^ Nothing -> Boss turn; Just spell -> Player turn
--      -> State       -- ^ current state
--      -> Maybe State -- ^ next state or error
-- step Nothing          s@State{..} | pNewHp <= 0 = Nothing -- Just Lost -- XXX Nothing?
--                                   | otherwise   = Just s{ pHp = pNewHp }
--   where armor = maximum (pArmor : map eArmor effs)
--         pNewHp | bDmg - armor < 1 = pHp - 1
--                | otherwise        = pHp - (bDmg - armor)
-- step (Just Spell{..}) s
--   = let s' = applyEffects s
--         s'' = s' { pHp = (pHp s') + spHeal
--                  , pMana = (pMana s') - spCost
--                  , pSpent = (pSpent s') + spCost
--                  , bHp = (bHp s') - spDmg
--                  , effs = effs' }
--         effs' | Just e <- spEffect = effs s' ++ [e]
--               | otherwise          = effs s'
--     in if spCost > pMana s' || effectAlreadyActive s spEffect
--        then Nothing
--        else if bHp s'' <= 0
--             then Just $! Won (pSpent s'')
--             else Just $! s''

effectAlreadyActive :: State -> Maybe Effect -> Bool
effectAlreadyActive _         Nothing  = False
effectAlreadyActive State{..} (Just e) = eName e `elem` map eName effs

applyEffects s = go ([],s) (effs s)
  where
    go (effs',s') []     = s' { effs = effs' }
    go (effs',s') (e:es) = go (mappend effs' effs'',s'') es
      where
        (effs'',s'') = effect e s'

initial = State 50 0 500 0 58 9 []

play :: String -> Maybe State
play = go initial . (++[Nothing]) . intersperse Nothing . map (pure . spell)
  where
    go s []       = Just s
    go s (ms:mss) = do
      s' <- step ms s
      go s' mss

playAndLog :: String -> Maybe (State,String)
playAndLog spells = (\s -> (s,spells)) <$> play spells

play' :: State -> String -> Maybe (State,String)
play' is spells = (\s -> (s,spells)) <$> (go is . (++[Nothing]) . intersperse Nothing . map (pure . spell) $ spells)
  where
    go s []       = Just s
    go s (ms:mss) = do
      s' <- step ms s
      go s' mss

playAndLog' is spells = (\s -> (s,spells)) <$> play' is spells

example1 = State 10 0 250 0 13 8 []

solutionsFor is n
  = filter (\(s,_) -> case s of Won _ -> True; _ -> False)
  . catMaybes
  . map (play' is)
  $ boundedPlays n
