
import Text.Parsec
import Text.Parsec.String

import Data.List
import qualified Data.Set as S

main = do
  Right (rs,m) <- parseFromFile p "input.txt"
  print . S.size . molecules m $ rs

p = do { rs <- (do { r <- many1 letter ; string " => " ; l <- many1 letter
                   ; return (r,l)
                   } `endBy1` newline)
       ; newline ; m <- many1 letter ; eof
       ; return (rs, m)
       }

molecules m = foldl' (\s -> S.union s . S.fromList . replace m) S.empty

replace m (from,to) = let
  is = filter (completeMatch m from) $ findIndices (head from ==) m
  n = length from
  replaceAt i = let (ls,rs) = splitAt i m in ls ++ to ++ drop n rs
  in replaceAt <$> is

completeMatch m from i = and $ zipWith (==) from (drop i m)
