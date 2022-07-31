{-# LANGUAGE TemplateHaskell #-}
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.FileEmbed (embedFile)
import Data.ByteString.UTF8 (toString)
import Data.Function ((&))
import System.Random (randomRIO)

data Record = Record { key   :: String
                     , value :: String}

records :: [Record]
records = $(embedFile "Records.csv")
    & tail . lines . toString
    & map (splitOn ",")
    & map (\[x, y] -> Record y x)

getRandomIndices :: [Int] -> IO [Int]
getRandomIndices xs = case length xs of
    0 -> getRandomIndex >>= getRandomIndices . (: [])
    4 -> (: xs) <$> randomRIO (1, 4)
    _ -> getRandomIndex >>= \x ->
            if x `elem` xs then 
                getRandomIndices xs
            else 
                getRandomIndices $ x : xs
    where   getRandomIndex :: IO Int
            getRandomIndex = randomRIO (0, length records - 1)

qna :: IO Bool
qna = do
    indices <- getRandomIndices []
    let correctIndex = head indices
    let correctRecord = records !! (indices !! correctIndex)
    let correctKey = key correctRecord
    let correctAnswer = show correctIndex
    let questionValue = value correctRecord
    putStrLn $ "\n\"" ++ questionValue ++ "\" is mapped to :"
    let choices = zip [1..] . map (key . (records !!)) $ tail indices
    let showChoices = intercalate "\t" . map (\(x, y) -> "[" ++ show x ++ "]" ++ y) $ choices
    putStrLn showChoices
    answer <- getChar
    _ <- getLine
    if [answer] == correctAnswer || answer == head correctKey then 
        putStrLn "Correct!" >> return True
    else 
        putStrLn ("Incorrect! The correct answer is [" ++ correctAnswer ++ "]" ++ correctKey) >> return False

quiz :: [Bool] -> Int -> IO ()
quiz xs n = case n of
    0 -> putStrLn $ "\nComplete the quiz! The accuracy is " ++ accuracy ++ "%"
    _ -> qna >>= \x -> quiz (x : xs) (n - 1)
    where accuracy = show . round $ (fromIntegral . length . filter (== True) $ xs) / (fromIntegral . length $ xs) * 100.0


main :: IO ()
main = do
    putStrLn "Double Pinyin Quiz!\nHow many tests would you like?\n[1]5\t[2]10\t[3]20\t[4]50\t[(Other Input)]Quit"
    answer <- getChar
    _ <- getLine
    case answer of
        '1' -> quiz [] 5
        '2' -> quiz [] 10
        '3' -> quiz [] 20
        '4' -> quiz [] 50
        _ -> return ()