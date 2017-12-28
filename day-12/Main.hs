{-# LANGUAGE TypeApplications, LambdaCase #-}
import Text.Parsec
import Text.Parsec.String
import Control.Arrow (second)

main = do
  Right r <- parseFromFile thing "input.txt"
  print . solve $ r
  print . solve . prune $ r

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

prune (O as)
  | "red" `elem` map (\case (_,S s) -> s; o -> "") as = O []
  | otherwise = O $ map (second prune) as
prune (A xs) = A $ map prune xs
prune x = x

solve (N x ) = x
solve (S _ ) = 0
solve (A xs) = sum . map solve $ xs
solve (O as) = sum . map (solve . snd) $ as
