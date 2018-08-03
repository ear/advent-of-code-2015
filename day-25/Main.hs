{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Text.Read     ( readMaybe )
import Data.Char     ( isDigit   )
import Data.Maybe    ( catMaybes )
import Control.Monad ( forM_     )

group_order, m, m_inv, m_ord :: Integer

-- | The underlying group is Zn/Z with n = 33554393 (which is prime)
group_order = 33554393

-- | m is also prime
m = 252533

-- | m inverse: (m * m_inv) `mod` group_order == 1
m_inv = 3786434

-- | order of m: (m^m_ord) `mod` group_order == 1
m_ord = 16777196

-- | Fast exponentiation by squaring
pow :: Integer -> Integer -> Integer
pow m e = pow' e
  where
    pow' 0 = 1
    pow' 1 = m `mod` group_order
    pow' !e
      | e `mod` 2 == 0 = (pow' (e`div`2))^2 `mod` group_order
      | otherwise      = (pow' (e-1)) * m `mod` group_order

test_group_props = do
  print $ (m * m_inv) `divMod` group_order
  print $ pow m m_ord

{-

> test_table 6
   .   1   2   3   4   5   6
   1   0   2   5   9  14  20
   2   1   4   8  13  19  26
   3   3   7  12  18  25  33
   4   6  11  17  24  32  41
   5  10  16  23  31  40  50
   6  15  22  30  39  49  60

-}

-- | The first column is the triangular numbers
triangular_numbers :: [Integer]
triangular_numbers = 0 : zipWith (+) [1..] triangular_numbers

-- | To get the i-th element of column j travel back diagonally to column 1
--   and sum the number of elements of the diagonal.
table :: Integer -> Integer -> Integer
table i 1 = triangular_numbers !! (fromIntegral $ i - 1)
table i j = (j - 1) + table (i + j - 1) 1

test_table :: Integer -> Integer -> IO ()
test_table n_min n_max
  | n_min < 1 || n_min > n_max = error "wrong bounds"
  | otherwise = do
    p "."
    mapM_ (p . show) [n_min .. n_max]
    nl
    forM_ [n_min .. n_max] $ \i -> do
      p (show i)
      forM_ [n_min .. n_max] $ \j -> do
        p (show $ table i j)
      nl
  where
    width = 2 + (length $ show $ table n_max n_max)
    p = putStr . padL width
    padL n s | length s < n = replicate (n - length s) ' ' ++ s
             | otherwise    = s
    nl = putChar '\n'

test_table' :: (Integer -> Integer)
            -> (Integer,Integer)
            -> (Integer,Integer)
            -> IO ()
test_table' f (xm,xM) (ym,yM)
  | xm < 1 || ym < 1 || xm > xM || ym > yM = error "wrong bounds"
  | otherwise = do
    p "."
    mapM_ (p . show) [xm..xM]
    nl
    forM_ [ym..yM] $ \i -> do
      p (show i)
      forM_ [xm..xM] $ \j -> do
        p (show $ f $ table i j)
      nl
  where
    width = 2 + (length $ show $ table xM yM)
    p = putStr . padL width
    padL n s | length s < n = replicate (n - length s) ' ' ++ s
             | otherwise    = s
    nl = putChar '\n'

main :: IO ()
main = do
  (i,j) <- readInput
  test_table' code (j-3,j+3) (i-3,i+3)
  print $ code (table i j)
  where
    code = (`mod` group_order) . (20151125 *) . pow m

readInput :: IO (Integer,Integer)
readInput = do
  ws <- map (filter isDigit) <$> words <$> readFile "input.txt"
  let [i,j] = catMaybes $ readMaybe @Integer <$> ws
  return (i,j)
