module Main where

import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import System.Environment (getArgs)

data Tree = Node Int Double Tree Tree
    | Leaf String
    deriving (Show)

data TreeLine = TreeNode Int (Int, Double)
    | TreeLeaf Int String
    deriving (Show)

-- funkce pro nacteni vstupniho souboru
loadFile :: FilePath -> IO [String]
loadFile path = do
    content <- readFile path
    return (lines content)

-- ocisteni radku od mezer
trimStart :: String -> String
trimStart = dropWhile (== ' ')

-- rozpoznani Node
isNode :: String -> Bool
isNode line = "Node:" `isPrefixOf` trimStart line

-- rozpoznani Leaf
isLeaf :: String -> Bool
isLeaf line = "Leaf:" `isPrefixOf` trimStart line

countIndent :: String -> Int
countIndent = length . takeWhile (== ' ')

parseNode :: String -> (Int, Double)
parseNode line =
    case splitOn ", " (drop 6 (trimStart line)) of
        [idxStr, threshStr] -> (read idxStr, read threshStr)
        _ -> error "Neplatny format uzlu"

parseLeaf :: String -> String
parseLeaf line = drop 6  (trimStart line)

parseLine :: String -> TreeLine
parseLine line
    | isNode line = TreeNode indent (parseNode line)
    | isLeaf line = TreeLeaf indent (parseLeaf line)
    | otherwise = error "Neplatny radek vstupniho souboru."
    where
        indent = countIndent line

-- vytvari korenovy uzel
buildTree :: [TreeLine] -> (Tree, [TreeLine])
buildTree (TreeNode indent (index, threshold) : rest) =
    let (leftSubTree, rest1) = buildSubTree indent rest
        (rightSubTree, rest2) = buildSubTree indent rest1
    in (Node index threshold leftSubTree rightSubTree, rest2)
buildTree (TreeLeaf indent label : rest) =
        (Leaf label, rest)
buildTree [] = error "Neocekavany konec souboru"

buildSubTree :: Int -> [TreeLine] -> (Tree, [TreeLine])
buildSubTree parentIndent (x@(TreeNode indent _):xs)
    | indent > parentIndent = buildTree (x:xs)
    | otherwise = (Leaf "ERROR", x:xs)
buildSubTree parentIndent (x@(TreeLeaf indent _):xs)
    | indent > parentIndent = buildTree (x:xs)
    | otherwise = (Leaf "ERROR", x:xs)
buildSubTree _ [] = error "Neocekavany konec souboru"

main :: IO ()
main = do
    let treeLines = map parseLine [ "Node: 0, 5.5", "  Leaf: TridaA", "  Node: 1, 3.0", "    Leaf: TridaB", "    Leaf: TridaC"]
    let (tree, _) = buildTree treeLines
    print tree
