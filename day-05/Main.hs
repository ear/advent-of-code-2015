import Data.List

main :: IO ()
main = print =<< length . filter nice . lines <$> readFile "input.txt"

nice :: String -> Bool
nice xs = and [blacklist,vowels,twice]
  where
    blacklist = all (`notElem` map (take 2) (tails xs)) ["ab","cd","pq","xy"]
    vowels = (>2) . length . filter (`elem` "aeoiu") $ xs
    twice = any ((>1) . length) . groupBy (==) $ xs
