import Data.List
f n = cycle $ 10*(n+1) : replicate n 0
g n = zipWith (\x y -> let z = x+y in seq z z) (f n) (0 : g (n+1))
s = g 0
main = print . findIndex (>=29000000) $ s
