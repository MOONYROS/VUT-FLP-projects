-- vraci soucet seznamu
-- z=akumulator, x je prvni prvek seznamu, z+x rika, co se ma vracet, 0 je pocatecni stav akumulatoru,, seznam je parametr
-- foldl (\z x -> z+x) 0 [1, 2, 3, 4]

-- vraci soucet kazdeho druheho prvku seznamu
-- ukol je nadefinovat f
-- foldl f 0 [1,2,3,4]
-- sum2 xs = fst $ foldl f (0, False) xs
    -- where
        -- ELEGANTNEJSI RESENI - muzu si prepsat True -> 1, False -> 0
        -- (s, _) = foldl f (0, False) xs
        -- f (s, False) x = (s, False)
        -- f (s, False) x = (s+x, True)

        -- NE TOLIK ELEGANTNI
        -- (s, _) = foldl f (0, 1) xs
        -- f (s, i) x
        --     | even i = (s+x. i+1)
        --     | otherwise (s, i+1)

data Stack a = Stack [a] deriving (Show)

push :: a -> Stack a -> Stack a
push top (Stack rest) = Stack (top : rest)

pop :: Stack a -> (a, Stack a)
pop (Stack (top:rest)) = (top, Stack rest)

-- musi se udelat v kostantni casove slozitosti
len :: Stack a -> Int
-- takhle by se to delalo rekurzivne
len (Stack xs) = f xs
    where
        f [] = 0
        f (x:xs) = 1 + f xs
-- pokud bych to mel mit s konstantni casovou slozitosti je, ze bych si tu delku ulozil do toho 'data Stack'
-- je to pojeb af

main :: IO ()
main = do
    print "Done."

