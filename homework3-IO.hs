import Data.List;
import Data.Time.Clock
import Data.Time.Calendar

hcatNoArgs :: IO ()
hcatNoArgs = do
    getLine >>= (\x -> putStrLn x)

hcat :: FilePath -> IO ()
hcat path = do
    readFile path >>= (\x -> putStrLn x)

printWithColor :: String -> String -> IO ()
printWithColor _ "" = putStrLn ""
printWithColor pattern s = if isPrefixOf pattern s
    then do
        putStr $ "\x1b[31m" ++ pattern ++ "\x1b[0m"
        printWithColor pattern $ snd $ splitAt (length pattern) s
    else do
        putStr $ [head s]
        printWithColor pattern $ tail s

grepPrint :: String -> [String] -> IO ()
grepPrint _ [] = return ()
grepPrint pattern (s:ss) = do
    printWithColor pattern s
    grepPrint pattern ss

hgrepNoArgs :: String -> IO ()
hgrepNoArgs pattern = do
    input <- getLine
    grepPrint pattern [input]

splitBy :: (Char -> Bool) -> String -> [String]
splitBy pat s = case dropWhile pat s of
    "" -> []
    s' -> w : splitBy pat s''
          where (w, s'') = break pat s'

hgrep :: String -> FilePath -> IO ()
hgrep pattern path = do    
    input <- readFile path >>= return . splitBy (== '\n')
    grepPrint pattern input

printDays :: Int -> Int -> Int -> IO ()
printDays iterDay curDay maxDay = if iterDay > maxDay then return ()
    else do
        if iterDay == curDay then putStr $ "\x1b[30m" ++ (show iterDay) ++ "\x1b[0m" else putStr $ show iterDay
        if iterDay `mod` 7 == 0 then putStrLn "" else putStr "\t"
        printDays (iterDay + 1) curDay maxDay

hncal :: IO ()
hncal = do
    (year, month, day) <- getCurrentTime >>= return . toGregorian . utctDay
    case month of
        1 -> putStrLn $ "\t\tJanuary " ++ (show year)
        2 -> putStrLn $ "\t\tFebruary " ++ (show year)
        3 -> putStrLn $ "\t\tMarch " ++ (show year)
        4 -> putStrLn $ "\t\tApril " ++ (show year)
        5 -> putStrLn $ "\t\tMay " ++ (show year)
        6 -> putStrLn $ "\t\tJune " ++ (show year)
        7 -> putStrLn $ "\t\tJuly " ++ (show year)
        8 -> putStrLn $ "\t\tAugust " ++ (show year)
        9 -> putStrLn $ "\t\tSeptember " ++ (show year)
        10 -> putStrLn $ "\t\tOctober " ++ (show year)
        11 -> putStrLn $ "\t\tNovember " ++ (show year)
        12 -> putStrLn $ "\t\tDecember " ++ (show year)
    printDays 1 day (gregorianMonthLength year month)
    putStrLn ""