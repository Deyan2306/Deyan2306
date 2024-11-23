dummyGetLine :: IO String
dummyGetLine =
    return "Im not really doing anything..."
    
-- desktopFilePath = "/home/local/Desktop/file.txt"

main :: IO()
main = do

    -- writeFile desktopFilePath "This is some text" 

    putStrLn "Hello World"
    putStrLn "This is the second line"
    putStrLn "And this is the third"
    
    -- line <- getLine
    line <- dummyGetLine
    putStrLn ("You said: " ++ line)
    
-- putStrLn :: String -> IO ()
-- getLine :: IO String
-- print :: (Show a) => a -> IO ()
-- readFile :: FilePath -> IO String
-- writeFile :: FilePath -> String -> IO ()
-- appendFile :: FilePath -> String -> IO ()
