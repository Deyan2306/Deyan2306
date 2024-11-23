reverseList :: [Int] -> [Int]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

duplicateElements :: [a] -> [a]
duplicateElements [] = []
duplicateElements (x:xs) = x : x : duplicateElements xs

removeEveryNth :: Int -> [a] -> [a]
removeEveryNth _ [] = []
removeEveryNth n xs count = removeEveryNth' m xs 1

removeEveryNth' :: Int -> [a] -> Int -> [a]
removeEveryNth' _ [] _ = [] 
removeEveryNth' n xs count
    | count == n    = removeEveryNth' n (drop 1 xs) 1
    | otherwise     = take 1 xs ++ removeEveryNth' n (drop 1 xs) (count + 1)

fibonacciUpTo :: Int -> [Int]
fibonacciUpTo n = fibonacciHelper 0 1 n
  where
    fibonacciHelper :: Int -> Int -> Int -> [Int]
    fibonacciHelper a b limit
      | a > limit = [] 
      | otherwise = a : fibonacciHelper b (a + b) limit

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

factorialsUpTo :: Int -> [Int]
factorialsUpTo n = factorialHelper 0 n
  where
    factorialHelper :: Int -> Int -> [Int]
    factorialHelper current limit
      | current > limit = [] 
      | otherwise = factorial current : factorialHelper (current + 1) limit

main :: IO()
main = do
    val <- getLine
    let parsed = read ("[" ++ val ++ "]" ) :: [Int]
    
    putStrLn (show (reverseList parsed))