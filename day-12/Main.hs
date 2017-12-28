{-# LANGUAGE TypeApplications #-}
import Text.Parsec
import Text.Parsec.String

main = do
  Right r <- parseFromFile p "input.txt"
  print . sum . map (read @Int) $ r

p = do { skipMany (noneOf "-0123456789")
       ; n <- (:) <$> option ' ' (char '-') <*> many1 digit
       ; skipMany (noneOf "-0123456789")
       ; return n }
    `sepBy1`
    (many $ noneOf "-0123456789")
