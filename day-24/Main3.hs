{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Ord   ( comparing    )
import Data.List  ( sort, sortBy )
import Data.Maybe ( listToMaybe  )

data Packages = Pkgs { pkgCount, pkgSum, pkgProd :: !Int }
  deriving (Eq, Show)

noPkgs :: Packages
noPkgs = Pkgs { pkgCount = 0, pkgSum = 0, pkgProd = 1 }

addPkg :: Int -> Packages -> Packages
addPkg p pkgs = pkgs
  { pkgCount = pkgCount pkgs + 1
  , pkgSum   =   pkgSum pkgs + p
  , pkgProd  =  pkgProd pkgs * p }

instance Ord Packages where
  compare = comparing pkgCount <> comparing pkgSum <> comparing pkgProd

search :: Int -> [Int] -> Maybe Int
search n ps0 = listToMaybe $
  do (pkgs,ps1) <- sortBy (comparing fst) (go ps0)
     moreGroups (n-1) ps1
     return (pkgProd pkgs)

  where
    goal = sum ps0 `div` n

    go = go' noPkgs [] . sort

    go' :: Packages -> [Int] -> [Int] -> [(Packages,[Int])]
    go' a qs _ | pkgSum a == goal = [(a,qs)]
    go' _ _ [] = []
    go' a _ (p:_) | pkgSum (addPkg p a) > goal = []
    go' a qs (p:ps) = go' (addPkg p a) qs ps ++ go' a (p:qs) ps

    moreGroups 1 _ = [()]
    moreGroups i ps1 =
      do (_,ps2) <- go ps1
         moreGroups (i-1) ps2

main :: IO ()
main = do
  weights <- map (read @Int) <$> lines <$> readFile "input.txt"
  print $ search 3 weights
  print $ search 4 weights
