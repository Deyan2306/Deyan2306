log2 :: Int -> Int
log2 x
    | x == 1    = 0
    | x > 1     = 1 + log2 (x `div` 2)
    | otherwise = error "Input must be greater than 0"

factorial :: Int -> Int
factorial n = findFactorial n 1 1

findFactorial :: Int -> Int -> Int -> Int
findFactorial n initial i
    | i > n     = initial
    | otherwise = findFactorial n (initial * i) (i + 1) 

findFibonacci :: Int -> Int -> Int -> Int
findFibonacci n initialValue prevValue index
    | index >= n    = initialValue
    | otherwise     = findFibonacci n (initialValue + prevValue) initialValue (index + 1)
    
fibonacci :: Int -> Int
fibonacci n = findFibonacci n 1 0 1

asterixStringRow :: Int -> String
asterixStringRow n = replicate n '*'

printTriangle :: Int -> IO()
printTriangle 0 = return ()
printTriangle n = do
    putStrLn (asterixStringRow n)
    printTriangle (n - 1)

main :: IO ()
main = do
    input <- getLine
    let parsed = read input :: Int
    
    putStrLn (show (factorial parsed))
