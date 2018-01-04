{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-do-bind
    -Wno-unused-imports -Wno-type-defaults #-}
import Text.Parsec
import Text.Parsec.String

import Data.List
import qualified Data.Set as S

import Debug.Trace
import Control.Arrow
import Data.Maybe
import Data.Ord
import Data.Graph
import Data.Function

main = do
  Right (rs,m) <- parseFromFile p "test.txt"
  -- print . S.size . molecules m $ rs
  let swap (a,b) = (b,a)
      rs' = reverse $ sortBy (comparing (length . fst)) $ swap <$> rs
  -- print . search m (length m) rs 0 [] 0 $ "e"
  -- print . search m (length m) rs 0 $ "e"
  print rs'
  -- print . search "e" rs' 0 $ m
  -- let g = graphFromEdges


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



search needle rs n m
  | needle == m = traceShowId $! [n]
  | otherwise = search needle rs (n+1) =<< (replace m =<< rs)



-- search needle _ _  n m | needle == m = traceShowId $! [n]
-- search needle l rs n m = let
--     !expansions = filter ((<= l) . length) $ replace m =<< rs
--     in search needle l rs (n+1) =<< expansions

-- search needle _ _  n _  _ m | needle == m = [n]
-- search needle l rs n ns i m
--   | i == length rs = ns
--   | otherwise      = let
--     !r = rs !! i
--     !expansions = filter ((<= l) . length) $ replace m r
--     !ns' = search needle l rs (n+1) [] 0 =<< expansions
--     in search needle l rs n (ns ++ ns') (i+1) m
