{-# LANGUAGE LambdaCase #-}

main :: IO ()
main = do
  boxes <- parse <$> readFile "input.txt"
  print $ paper boxes
  print $ ribbon boxes

type Boxes = [(Int,Int,Int)]

parse :: String -> Boxes
parse = map (triple . words . map (\case 'x' -> ' '; x -> x)) . lines
  where triple [x,y,z] = (read x,read y,read z); triple _ = error "parse error"

paper :: Boxes -> Int
paper boxes = sum [ area triple + areaMinSide triple | triple <- boxes ]
  where
    area (x,y,z) = sum $ map (2*) [x*y,y*z,z*x]
    areaMinSide (x,y,z) = minimum [x*y,y*z,z*x]

ribbon :: Boxes -> Int
ribbon boxes = sum [ volume triple + perimeterMinSide triple | triple <- boxes ]
  where
    volume (x,y,z) = x*y*z
    perimeterMinSide (x,y,z) = minimum $ map (2*) [x+y,y+z,z+x]
