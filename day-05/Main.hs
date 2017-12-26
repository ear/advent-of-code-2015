{-# LANGUAGE LambdaCase #-}
import Data.List
import Control.Arrow

main :: IO ()
main = do
  ws <- lines <$> readFile "input.txt"
  print . length . filter nice $ ws
  print . length . filter nice2 $ ws

nice :: String -> Bool
nice xs = and [blacklist,vowels,twice]
  where
    blacklist = all (`notElem` map (take 2) (tails xs)) ["ab","cd","pq","xy"]
    vowels = (>2) . length . filter (`elem` "aeoiu") $ xs
    twice = any ((>1) . length) . groupBy (==) $ xs

nice2 xs = and [nonoverpair,eye]
  where
    nonoverpair
      = or [ xy `elem` ls || xs `elem` rs
           | n <- [2 .. length xs - 2]
           , let (l,(xy,r)) = (id *** splitAt 2) $ splitAt n xs
           , let ls = map (take 2) (tails l)
           , let rs = map (take 2) (tails r) ]
    eye = any (\case [a,_,c] -> a==c; _ -> False) $ map (take 3) (tails xs)
