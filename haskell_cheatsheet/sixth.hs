-- Задача 1: Генериране на математически израз (формат (((a + b) + c) + d))
generateExpression1 :: [Int] -> String
generateExpression1 [] = ""
generateExpression1 [x] = show x
generateExpression1 (x:xs) = "(" ++ show x ++ " + " ++ generateExpression1 xs ++ ")"

-- Задача 2: Генериране на математически израз (формат (a + (b + (c + d))))
generateExpression2 :: [Int] -> String
generateExpression2 [] = ""
generateExpression2 [x] = show x
generateExpression2 (x:xs) = show x ++ " + (" ++ generateExpression2 xs ++ ")"

-- Задача 3: Компресиране на списък (премахва последователни повтарящи се елементи)
compressList :: Eq a => [a] -> [a]
compressList = foldr (\x acc -> if null acc || x /= head acc then x : acc else acc) []

-- Задача 4: Дупликация на списъчни елементи
duplicateElements :: [a] -> [a]
duplicateElements = foldr (\x acc -> x : x : acc) []

-- Задача 5: Репликация на списъчни елементи
replicateElements :: [a] -> Int -> [a]
replicateElements xs n = concatMap (replicate n) xs

-- Задача 6: Отрязване на списък (вземане на елементи между начален и краен индекс)
sliceList :: [a] -> Int -> Int -> [a]
sliceList xs start end = take (end - start) (drop start xs)