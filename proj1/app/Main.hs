module Main where

import Data.List (isPrefixOf, minimumBy, maximumBy, sort, group)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Ord (comparing)
import System.Environment (getArgs)

data Tree = Node Int Double Tree Tree
    | Leaf String
    deriving (Show)

data TreeLine = TreeNode Int (Int, Double)
    | TreeLeaf Int String
    deriving (Show)

expectedIndentStep :: Int
expectedIndentStep = 2

-- funkce pro nacteni vstupniho souboru
loadFile :: FilePath -> IO [String]
loadFile path = do
    content <- readFile path
    return (lines content)

-- ocisteni radku od mezer
trimStart :: String -> String
trimStart = dropWhile (== ' ')

-- =======================================
-- =========== TREE VALIDATION ===========
-- =======================================

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
    let (leftSubTree, rest1) = buildSubTree (indent + expectedIndentStep) rest
        (rightSubTree, rest2) = buildSubTree (indent + expectedIndentStep) rest1
    in (Node index threshold leftSubTree rightSubTree, rest2)
buildTree (TreeLeaf _ label : rest) =
    (Leaf label, rest)
buildTree [] = error "Neocekavany konec souboru"

buildSubTree :: Int -> [TreeLine] -> (Tree, [TreeLine])
buildSubTree _ [] = error "Neocekavany konec souboru - chybi potomek uzlu."
buildSubTree expectedIndent treeLines@(x:_) =
    case x of
        TreeNode indent _ ->
            if indent == expectedIndent
                then buildTree treeLines
                else error "Nespravna indentace uzlu."
        TreeLeaf indent _ ->
            if indent == expectedIndent
                then buildTree treeLines
                else error "Nespravna indentace listu."

-- =======================================
-- ========= DATA CLASSIFICATION =========
-- =======================================

-- proparsuji vstupni radek, rozdelim podle carek a pomoci map read premapuji String na Double
parseData :: String -> [Double]
parseData line = map read (splitOn "," (trimStart line))

classifyData :: Tree -> [Double] -> String
classifyData (Leaf className) _ = className
classifyData (Node index threshold leftTree rightTree) features = 
    let feature = features !! index
    in if feature < threshold
        then classifyData leftTree features
        else classifyData rightTree features

-- =======================================
-- ============ TREE TRAINING ============
-- =======================================

parseTrainData :: String -> ([Double], String)
parseTrainData line =
    let parts = splitOn "," (trimStart line)
        features = map read (init parts)
        label = last parts
    in (features, label)

-- vypocet gini indexu
calculateGini :: [String] -> Double
calculateGini classes
    -- pro 0 trid je gini index 0
    | null classes = 0.0
    -- pro vice trid spocitam podle vzorce 1 - sum(p_i^2)
    | otherwise = 1.0 - sum [((fromIntegral count) / total) ^ (2 :: Int) | count <- counts]
    where
        -- spocitam si cetnosti pro vsechny tridy
        counts = map length $ group $ sort classes -- spocitani velikosti skupin
        total = fromIntegral $ length classes -- celkovy pocet prvku

-- TODO: Udelat trenovani stromu a vyuzit Gini index
findBestSplit :: [([Double], String)] -> (Int, Double, Double)
findBestSplit = error "Function under construction."

-- zatim nemam currentDepth, minSamples, ani maxDepth
trainTree :: [([Double], String)] -> Tree
trainTree dataset
    | null dataset = error "Empty dataset!"
    -- TODO: osetrit a overit podminky
    | length (group (sort labels)) == 1 = Leaf (head labels) -- data jsou v jedne tride => Leaf
    | otherwise = Node featureIndex threshold (trainTree leftDataset) (trainTree rightDataset) -- data nemaji stejnou tridu => Node
    where
        -- seznam trid
        labels = map snd dataset

        -- najdeme nejlepsi rozdeleni
        (featureIndex, threshold, _) = findBestSplit dataset
        
        -- TODO: overit spravnost <, >=
        leftDataset = [(features, label) | (features, label) <- dataset, features !! featureIndex < threshold]
        rightDataset = [(features, label) | (features, label) <- dataset, features !! featureIndex >= threshold]


-- funkce pro prevod stromu na vystupni format
treeToOutput :: Tree -> [String]
treeToOutput tree = treeToOutputHelper tree 0 0
    where
        treeToOutputHelper :: Tree -> Int -> Int -> [String]
        treeToOutputHelper (Leaf label) _ _ = ["Leaf: " ++ label] -- pro list proste vypiseme tridu
        treeToOutputHelper (Node _ threshold left right) depth nodeId =
            ["Node: " ++ show nodeId ++ ", " ++ show threshold] ++ -- vypiseme informace o uzlu
            map (" " ++) (treeToOutputHelper left (depth + 1) (2 * nodeId + 1)) ++ -- levy podstrom s odsazenim
            map (" " ++) (treeToOutputHelper right (depth + 1) (2 * nodeId + 2)) -- pravy podstrom s odsazenim



-- ======================================
-- ================ MAIN ================
-- ======================================

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-1", treeFile, dataFile] -> do
            treeContent <- loadFile treeFile
            let treeLines = map parseLine treeContent
            let (tree, remainingLines) = buildTree treeLines
            if null remainingLines
                then do
                    dataContent <- loadFile dataFile
                    let parsedData = map parseData dataContent
                    let results = map (classifyData tree) parsedData
                    mapM_ putStrLn results
                else error "Neplatna struktura stromu - prebyvajici radky."
        ["-2", trainFile] -> do
            trainContent <- loadFile trainFile
            let trainData = map parseTrainData trainContent
            -- max depth: 5, min samples: 2, start depth 0
            let tree = buildCartTree trainData 5 2 0
            -- prevod stromu na vystupni format
            let output = treeToOutput tree
            mapM_ putStrLn output
        _ -> putStrLn $ unlines
            [ "\nPouziti programu flp-fun"
            , "========================\n"
            , "flp-fun -1 <soubor obsahujici strom> <soubor_obsahujici nove data>"
            , "\t- Provede klasifikaci dat (soubor 2) na zaklade rozhodovaciho stromu (soubor 1).\n"
            , "flp-fun -2 <soubor obsahujici trenovaci data>"
            , "\t- Natrenuje rozhodovaci strom na zaklade trenovacich dat."
            ]
