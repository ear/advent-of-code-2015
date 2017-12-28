{-# LANGUAGE ViewPatterns #-}
import Text.Parsec
import Text.Parsec.String

import Data.Word
import Data.Bits
import qualified Data.Map.Strict as M

import Control.Monad.Trans.State.Lazy

import System.Environment (getArgs)

main :: IO ()
main = do
  [s] <- getArgs
  Right (M.fromList -> book) <- parseFromFile booklet "input.txt"
  let res = evalState (eval (S s)) book
  print res
  let book2 = M.insert "b" (N res) book
  print $ evalState (eval (S s)) book2

data Expr
  = N Word16
  | S String
  | NOT Expr
  | OP String Expr Expr
  deriving Show

booklet = do { expr <- choice [ try $ string "NOT " >> NOT <$> lit
                              , try $ op
                              , try $ (N . read) <$> many1 digit
                              , lit ]
             ; string " -> "
             ; l <- many1 lower
             ; return (l,expr) }
          `sepBy` newline
  where
    lit = (try (N . read <$> many1 digit)) <|> (S <$> many1 letter)
    op = do { l1 <- lit
            ; name <- choice [ try $ between space space $ string "AND"
                             , try $ between space space $ string "OR"
                             , try $ between space space $ string "LSHIFT"
                             ,       between space space $ string "RSHIFT" ]
            ; l2 <- lit
            ; return $ OP name l1 l2 }

eval (N x) = pure x
eval (S s) = do Just e <- gets (M.lookup s)
                case e of
                  N _ -> eval e
                  _   -> do e' <- eval e
                            modify $ M.insert s (N e') -- memoization
                            return e'
eval (NOT e) = complement <$> eval e
eval (OP name e1 e2) = f <$> eval e1 <*> eval e2
  where
    f = case name of
      "AND"    -> (.&.)
      "OR"     -> (.|.)
      "LSHIFT" -> \x y -> shiftL x (fromIntegral y)
      "RSHIFT" -> \x y -> shiftR x (fromIntegral y)
