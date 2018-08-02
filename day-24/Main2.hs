{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Text.Parsec ( parse, sepEndBy1, many1, digit, space )

import Data.IntSet ( IntSet )
import qualified Data.IntSet as IntSet

import Data.List ( sortBy )
import Data.Ord ( comparing )

import Control.Monad ( guard )

main :: IO ()
main = do
  weights <- input
  let n = sum weights `div` 4
      state = fromList weights
      sols = loadFront n state >>= loadLeft n >>= loadTrunk n
      entangled = map (\s -> s { f_qe = product
                                      $ map (fromIntegral @IntSet.Key @Integer)
                                      $ IntSet.elems $ f s }) sols
      sorted = sortBy (comparing f_qe) entangled
  putStrLn "First 20 solutions, ordered by front's quantum entanglement (f_qe):"
  mapM_ print $ take 20 sorted
  putStrLn "Number of solutions computed to find the minima:"
  print $ length sorted

input :: IO [Int]
input = do
  Right ns <- parse p "" <$> readFile "input.txt"
  return ns
    where
      p = sepEndBy1 (read @Int <$> (many1 digit)) space

data State = S
  { f    :: IntSet  -- ^ front
  , l    :: IntSet  -- ^ left
  , r    :: IntSet  -- ^ right
  , t    :: IntSet  -- ^ trunk
  , f_s  :: Int     -- ^ sum of front
  , l_s  :: Int     -- ^ sum of left
  , r_s  :: Int     -- ^ sum of right
  , t_s  :: Int     -- ^ sum of trunk
  , f_qe :: Integer -- ^ front's quantum entanglement
  }
  deriving Show

fromList :: [Int] -> State
fromList ns = S
  { f    = IntSet.empty
  , l    = IntSet.empty
  , r    = IntSet.fromList ns
  , t    = IntSet.empty
  , f_s  = 0
  , l_s  = 0
  , r_s  = sum ns
  , t_s  = 0
  , f_qe = 0
  }

loadFront :: Int -> State -> [State]
loadFront n s = do
  way <- loadFront' 0 n s
  guard $ (sum $ IntSet.toList $ r way) >= (2*n)
  return way

loadFront' :: Int -> Int -> State -> [State]
loadFront' lower n s = do
  x <- IntSet.toAscList (r s)
  guard (x > lower)
  let s' = s { f = IntSet.insert x (f s)
             , r = IntSet.delete x (r s)
             , f_s = (f_s s) + x
             , r_s = (r_s s) - x }
  if x == n
    then return s'
    else do
      guard (n >= x)
      loadFront' x (n-x) s'

loadLeft :: Int -> State -> [State]
loadLeft n s = take 1 $ loadLeft' 0 n s -- no need to find ALL l/r splits!

loadLeft' :: Int -> Int -> State -> [State]
loadLeft' lower n s = do
  x <- IntSet.toDescList (r s)
  guard (x > lower)
  let s' = s { l = IntSet.insert x (l s)
             , r = IntSet.delete x (r s)
             , l_s = (l_s s) + x
             , r_s = (r_s s) - x }
  if x == n
    then return s'
    else do
      guard (n >= x)
      loadLeft' x (n-x) s'

loadTrunk :: Int -> State -> [State]
loadTrunk n s = take 1 $ loadTrunk' 0 n s -- no need to find ALL l/r splits!

loadTrunk' :: Int -> Int -> State -> [State]
loadTrunk' lower n s = do
  x <- IntSet.toDescList (r s)
  guard (x > lower)
  let s' = s { t = IntSet.insert x (t s)
             , r = IntSet.delete x (r s)
             , t_s = (t_s s) + x
             , r_s = (r_s s) - x }
  if x == n
    then return s'
    else do
      guard (n >= x)
      loadTrunk' x (n-x) s'

test :: IO ()
test = do
  let test_input = [1,3,4,5,6,7,8,10,97,100,101,102]
      n = sum test_input `div` 4
      state = fromList test_input
      sols = loadFront n state >>= loadLeft n >>= loadTrunk n
      entangled = map (\s -> s { f_qe = product
                                      $ map (fromIntegral @IntSet.Key @Integer)
                                      $ IntSet.elems $ f s }) sols
  mapM_ print entangled
