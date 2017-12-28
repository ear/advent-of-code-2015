{-# LANGUAGE TypeApplications #-}
import Text.Parsec
import Text.Parsec.String

main :: IO ()
main = do
  r <- parseFromFile p "input.txt"
  print r

p = do { c1 <- many1 letter
       ; string " to "
       ; c2 <- many1 letter
       ; string " = "
       ; d <- read @Int <$> many1 digit
       ; return (c1,c2,d)
       }
    `sepBy1` newline
