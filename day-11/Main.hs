{-# LANGUAGE LambdaCase #-}

import Data.Char
import Data.List

carry = (\case (True,xs) -> 1:xs; (False,xs) -> xs)
      . mapAccumR (\c d -> let n = if c then succ d else d
                           in (n>25,n `mod` 26))
                  False

next = reverse . map (uncurry (+)) . zip (1:repeat 0)
     . reverse . map (subtract (ord 'a') . ord)

p = map (chr . ((+) (ord 'a')))

step = p . carry . next

main = putStrLn . head . filter securityElf . iterate step $ "cqjxjnds"

securityElf xs = and [straight xs, banned xs, twopairs xs]
  where
    straight = any (\[x,y,z] -> y == succ x && z == succ y)
             . filter ((3==) . length) . map (take 3) . tails
    banned   = all (`notElem` "iol")
    twopairs = (>1) . length . nub . sort
             . filter (\[x,y] -> x == y)
             . filter ((2==) . length) . map (take 2) . tails
