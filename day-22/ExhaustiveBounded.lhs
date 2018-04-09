Simplification: ignore effects.

Just enumerate the possible sequences of payable costs given a maximum. Costs:

> es = [10,50,100]

Enumeration:

> f max
>   | max <= 0  = [[]]
>   | otherwise = let cs = filter (<= max) es
>                 in cs >>= (\c -> map (c:) . f . (max -) $ c)

Test:

> t = f 100

The catch is that some actual game "spells" do affect the maximum sometimes.
