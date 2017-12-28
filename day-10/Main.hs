import Data.List
import Control.Arrow

main :: IO ()
main = print
     . (length *** length)
     . ((!! 40) &&& (!! 50))
     . iterate (concatMap (\(x,y) -> [x,y])
                . unfoldr (\xs -> if null xs then Nothing else Just . rle $ xs))
     $ [1,3,2,1,1,3,1,1,1,2]

rle = (first (length &&& head)) . uncurry (span . (==)) . (head &&& id)
