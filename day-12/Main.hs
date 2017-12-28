{-# LANGUAGE TypeApplications #-}
import Text.Parsec
import Text.Parsec.String

main = do
  r <- parseFromFile thing "input.txt"
  print r

data JSON
  = N Int
  | S String
  | A [JSON]
  | O [(String,JSON)]
  deriving (Show)

thing = choice [ try array, try object, try str, number ]

array = A <$> between (char '[') (char ']') (thing `sepBy` (char ','))

object = O <$> between (char '{') (char '}') (association `sepBy` (char ','))

association = do { S s <- str ; char ':' ; v <- thing ; return (s,v) }

str = S <$> between (char '"') (char '"') (many1 (noneOf "\""))

number = N <$> read @Int <$> ((:) <$> option ' ' (char '-') <*> many1 digit)
