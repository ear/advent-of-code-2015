{-# LANGUAGE TypeApplications, LambdaCase #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-missing-signatures -Wno-unused-do-bind #-}

import Text.Parsec
import Text.Parsec.String
import Data.Ord
import Data.List
import Data.Monoid
import Data.Foldable
import Control.Arrow

main :: IO ()
main = do
  Right rs <- parseFromFile p "input.txt"
  let ps = uncurry zip
         . second (map $ getSum . fold . take 2503)
         . second (transpose . map frontrunners . transpose)
         . second (map $ tail . scanl (+) 0)
         . unzip . map go $ rs
  -- mapM_ print . take (length rs) $ ps
  print . maximumBy (comparing snd) $ ps

p = do { n <- many1 letter
       ; string " can fly "
       ; v <- read @Int <$> many1 digit
       ; string " km/s for "
       ; t <- read @Int <$> many1 digit
       ; string " seconds, but then must rest for "
       ; tr <- read @Int <$> many1 digit
       ; string " seconds."
       ; return (n,v,t,tr)
       }
    `sepBy` newline

go (name,v,t,tr) = (name, cycle $ (replicate t v) ++ replicate tr 0)

frontrunners = map (\case True -> Sum 1; False -> Sum 0)
             . uncurry (map . (==))
             . (maximum &&& id)
