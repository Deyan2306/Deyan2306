doubleVal :: Int -> Int
doubleVal x = x * 2

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

biggestOfThree :: Int -> Int -> Int -> Int
biggestOfThree x y z
    | x >= y && x >= z  = x
    | y >= x && y >= z  = y
    | otherwise         = z

addOne :: Int -> Int
addOne x = x + 1

removeOne :: Int -> Int
removeOne x = x - 1

runF :: (Int -> Int) -> Int -> Int
runF f n = f n

factoriel :: Int -> Int
factoriel n
    | n <= 1    = 1 
    | otherwise = n * (factoriel (n - 1))

fibonacci :: Int -> Int
fibonacci n
    | n == 1    = 1
    | n == 2    = 1
    | otherwise = fibonacci (n - 1) + fibonacci (n - 2)

main :: IO ()
main = do
    val <- getLine
    let parsed = read val :: Int
    
    putStrLn("Doubled Value is: " ++ show (doubleVal parsed))
    putStrLn("Is Even: " ++ show (isEven parsed))
    
    print (biggestOfThree 1 2 3)
    
    print (runF addOne 3)
    
    print (factoriel 5)
    print (fibonacci 10)
    
-- max a b
-- sqrt
