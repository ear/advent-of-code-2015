import Text.Parsec
import Text.Parsec.String

main :: IO ()
main = do
  Right book <- parseFromFile booklet "input.txt"
  mapM_ print book

data Expr
  = N Int
  | S String
  | NOT Expr
  | Op String Expr Expr
  deriving Show

booklet = do { expr <- choice [ try $ string "NOT " >> NOT <$> lit
            ;                , try $ op
            ;                , try $ (N . read) <$> many1 digit
            ;                , lit ]
            ; string " -> "
            ; l <- many1 lower
            ; return (l,expr) }
         `sepBy` newline
  where
    lit = (try (N . read <$> many1 digit)) <|> (S <$> many1 letter)
    op = do { l1 <- lit
            ; name <- choice [ try $ between space space $ string "AND"
            ;                , try $ between space space $ string "OR"
            ;                , try $ between space space $ string "LSHIFT"
            ;                ,       between space space $ string "RSHIFT" ]
            ; l2 <- lit
            ; return $ Op name l1 l2 }
