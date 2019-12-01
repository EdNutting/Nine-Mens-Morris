module Utils where
    data Option a = Option String String (IO a)

    askQuestion :: String -> [Option a] -> Bool -> IO a
    askQuestion question [] shouldPrintOptions =
        fail ("Question asked with no available options: '" ++ question ++ "'")
    askQuestion question options shouldPrintOptions =
      do putStrLn question
         if shouldPrintOptions
         then mapM printOption options
         else return []
         result <- getLine
         if result == "quit"
         then fail "User quit game"
         else selectOption result options
      where
        printOption :: Option a -> IO ()
        printOption (Option item description _)
            = putStrLn (item ++ " : " ++ description)

        -- selectOption :: String -> [Option a] -> IO a
        selectOption result [] =
            do putStrLn ("Invalid selection '" ++ result ++ "'.")
               askQuestion question options shouldPrintOptions
        selectOption result ((Option item _ action) : opts) =
            if result == item
            then action
            else selectOption result opts

    updateAt :: Int -> (a -> a) -> [a] -> [a]
    updateAt _ _ [] = []
    updateAt 0 f (x : xs) = f x : xs
    updateAt n f (x : xs) = x : updateAt (n-1) f xs
