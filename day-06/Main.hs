import Text.Parsec
import Text.Parsec.String

main :: IO ()
main = do
  Right xs <- parseFromFile pInput "input.txt"
  print xs

data Instruction = On     (Int,Int) (Int,Int)
                 | Off    (Int,Int) (Int,Int)
                 | Toggle (Int,Int) (Int,Int)
  deriving (Show, Eq)

pInput = do { i <- instruction ; space
            ; p1 <- pair ; string " through " ; p2 <- pair
            ; return $ i p1 p2 }
         `sepBy1` newline
  where
    instruction = choice [ try $ string "turn on"  >> return On
                         , try $ string "turn off" >> return Off
                         , try $ string "toggle"   >> return Toggle ]
    pair = do { [a,b] <- (many1 digit) `sepBy` (char ',')
              ; return (read a, read b) }
