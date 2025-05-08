main :: IO ()

sum2 :: [Int] -> Int
sum2 [] = 0
sum2 [x] = x
sum2 (x:y:xs) = x + sum2 xs

main = do
    print (sum2 [1, 2, 3])