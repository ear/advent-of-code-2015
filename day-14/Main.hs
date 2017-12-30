{-# LANGUAGE TypeApplications #-}

import Text.Parsec
import Text.Parsec.String
import Data.Ord
import Data.List
import Control.Arrow

main :: IO ()
main = do
  Right rs <- parseFromFile p "input.txt"
  let ps = uncurry zip
         . second (map $ head . drop 2503 . scanl (+) 0)
         . unzip . map go $ rs
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
