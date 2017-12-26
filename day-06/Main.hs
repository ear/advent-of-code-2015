import Data.List
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.String
import Control.Monad

main :: IO ()
main = do
  Right xs <- parseFromFile pInput "input.txt"
  print . S.size . foldl' eval (S.empty) $ xs
  print . getSum . foldMap Sum . foldl' eval2 (M.empty) $ xs

data Switch = On | Off | Toggle  deriving (Show, Eq)

data Instruction = I Switch (Int,Int) (Int,Int)  deriving (Show, Eq)

pInput = do { i <- instruction ; space
            ; p1 <- pair ; string " through " ; p2 <- pair
            ; return $ i p1 p2 }
         `sepBy1` newline
  where
    instruction = choice [ try $ string "turn on"  >> return (I On)
                         , try $ string "turn off" >> return (I Off)
                         , try $ string "toggle"   >> return (I Toggle) ]
    pair = do { [a,b] <- (many1 digit) `sepBy` (char ',')
              ; return (read a, read b) }

eval s (I i (xm,ym) (xM,yM)) = foldl' (op i) s $ liftM2 (,) [xm..xM] [ym..yM]

op i s c |      c `S.member` s  && (i == Off || i == Toggle) = c `S.delete` s
         | not (c `S.member` s) && (i == On  || i == Toggle) = c `S.insert` s
         | otherwise = s

eval2 m (I i (xm,ym) (xM,yM)) = foldl' (op2 i) m $ liftM2 (,) [xm..xM] [ym..yM]

incr n Nothing  = Just n
incr n (Just v) = Just (v+n)

decr Nothing = Nothing
decr (Just v) | v == 1 = Nothing | otherwise = Just (v-1)

op2 On     m c = M.alter (incr 1) c m
op2 Toggle m c = M.alter (incr 2) c m
op2 Off    m c = M.alter (decr  ) c m
