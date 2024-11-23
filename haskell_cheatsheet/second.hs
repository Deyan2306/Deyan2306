main :: IO ()
main = do
    first <- getLine
    second <- getLine
    
    let firstNumber = read first :: Int
    let secondNumber = read second :: Int
    
    putStrLn (show (firstNumber * secondNumber))