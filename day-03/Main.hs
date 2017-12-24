import Data.List
import Data.List.Split
import qualified Data.Set as S
import Control.Arrow

main = do
  directions <- readFile "input.txt"
  print . S.size . travel $ directions
  print . S.size . travel2 $ directions

travel :: String -> S.Set (Int,Int)
travel = snd . foldl' go ((0,0), S.singleton (0,0))

go ((x,y),s) '>' = (id &&& flip S.insert s) (x+1,y)
go ((x,y),s) '^' = (id &&& flip S.insert s) (x,y+1)
go ((x,y),s) '<' = (id &&& flip S.insert s) (x-1,y)
go ((x,y),s) 'v' = (id &&& flip S.insert s) (x,y-1)

travel2 :: String -> S.Set (Int,Int)
travel2 = uncurry S.union
        . (travel *** travel)
        . unzip . map (\[x,y] -> (x,y)) . chunksOf 2
