module Main where

import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import System.Environment (getArgs)

data Tree = Node Int Double Tree Tree
    | Leaf String
    deriving (Show)

-- funkce pro nacteni vstupniho souboru
loadFile :: FilePath -> IO [String]
loadFile path = do
    content <- readFile path
    return (lines content)

-- rozpoznani Node
isNode :: String -> Bool
isNode line = "Node:" `isPrefixOf` line

-- rozpoznani Leaf
isLeaf :: String -> Bool
isLeaf line = "Leaf:" `isPrefixOf` line

detectType :: String -> String
detectType line
    | isNode line = "Node"
    | isLeaf line = "Leaf"
    | otherwise = "Neznamy format"

countIndent :: String -> Int
countIndent = length . takeWhile (== ' ')

parseNode :: String -> (Int, Double)
parseNode line =
    case splitOn ", " (drop 6 line) of
        [idxStr, threshStr] -> (read idxStr, read threshStr)
        _ -> error "Neplatny format uzlu"

parseLeaf :: String -> String
parseLeaf line = drop 6 line

testTree :: Tree
testTree = Node 0 5.5
  (Leaf "TridaA")
  (Node 1 3.0
    (Leaf "TridaB")
    (Leaf "TridaC"))

main :: IO ()
main = do
    -- TEST 1 - nacitani souboru
    -- args <- getArgs
    -- case args of
    --     ["-1", inputFile] -> do
    --         inputLines <- loadFile inputFile
    --         putStrLn "Vstupni soubor:"
    --         mapM_ putStrLn inputLines
    --         putStrLn "\nRozpoznane typy radku:"
    --         mapM_ (\line -> putStrLn (line ++ " -> " ++ detectType line)) inputLines
    --     _ -> putStrLn "Pouziti: flp-fun -1 <soubor obsahujici strom>"
    -- TEST 2 - kontrola typu stromu
    -- print testTree
    -- TEST 3 - countIndent
    -- print $ countIndent "Node: 0, 5.5"
    -- print $ countIndent "  Leaf: TridaA"
    -- print $ countIndent "    Leaf: TridaB"
    -- TEST 4 - parsing
    print $ parseNode "Node: 0, 5.5"
    print $ parseLeaf "Leaf: TridaA"
