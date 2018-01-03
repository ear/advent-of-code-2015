{-# LANGUAGE LambdaCase, TypeApplications #-}

import Data.List
import qualified Data.Set as S

import Control.Arrow

newtype Grid = G (S.Set (Int,Int))

instance Show Grid where
  show (G s) = let
    showLight x y | (x,y) `S.member` s = '#' | otherwise = '.'
    in intercalate "\n" [ [ showLight x y | x <- [0..99] ]
                        | y <- [0..99] ]

instance Read Grid where
  readsPrec _ = (\g -> [(g,"")])
              . G . S.fromList . map fst
              . filter (\case (_,'#') -> True; _ -> False)
              . concatMap (\(y,xs) -> map (\(x,v) -> ((x,y),v)) xs)
              . map (second $ zip [(0 :: Int)..]) . zip [(0 :: Int)..] . lines

neighbors (x,y) =
  [(x+1,y),(x+1,y+1),(x,y+1),(x-1,y+1),(x-1,y),(x-1,y-1),(x,y-1),(x+1,y-1)]

light s c
  | c `S.member` s = case length . filter (`S.member` s) . neighbors $ c of
                       2 -> id
                       3 -> id
                       _ -> S.delete c
  | otherwise      = case length . filter (`S.member` s) . neighbors $ c of
                       3 -> S.insert c
                       _ -> id

coords = [ (x,y) | x <- [0..99], y <- [0..99] ]

step light (G s) = G $ foldl' (\f c -> light s c . f) id coords s

lightsOn (G s) = S.size s

main = do
  g <- read @Grid <$> readFile "input.txt"
  print . lightsOn . head . drop 100 . iterate (step light) $ g
  let (G s) = g; g' = G $ s `S.union` S.fromList stuck
  print . lightsOn . head . drop 100 . iterate (step light2) $ g'

stuck = [(0,0),(0,99),(99,99),(99,0)]

light2 s c
  | c `elem` stuck = id
  | otherwise      = light s c
