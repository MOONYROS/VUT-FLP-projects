main :: IO ()
main = do
    print (add 1 2)  -- Výstup: 3
    print (add 2 3)  -- Výstup: 5
    print (add 4 5)  -- Výstup: 9

-- bude fungovat jenom pro 1 2 a 2 3.
add 1 2 = 3
add 2 3 = 5

-- tohle uz je pro parametry a a b - vraci vyraz nejakeho datoveho typu
add a b = a + b

-- tohle je definice funkce (asi je to ten "$" operator)
f :: (a -> b) -> a -> b
apply f a = f a

-- priorita
-- vraci 6 - prioritu ma nasobeni
inc $ 1 * 5 
-- vraci 10 - prioritu ma inkrementace
inc 1 * 5

-- je dobre vedet, co je v prelude - zakladni knihovna
-- kolar se na to casto pta v pisemkach
-- rozhoduje o tom, jak dlouhy bude projekt. Muze to byt 8 radku, nebo 800 radku.

-- napriklad mame funkce: take, reverse, cycle, repeat, drop...
-- zajimavy je portal HOOGLE - hoogle.haskell.org
