main :: IO ()

-- compress "AAAABBBBCCCCCAAAAA" = "ABCA"
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) = if x == y then compress (y:xs) else x : compress (y:xs)

main = do
    print (compress "AAAAAAABBBBBCC")
    print (compress [1, 1, 1, 2, 2, 2])