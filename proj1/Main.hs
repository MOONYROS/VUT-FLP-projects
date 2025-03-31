{- |
Module: Main
Description: Decision tree implementation for BUT FIT - FLP (2024/2025)
Author: Ondrej Lukasek (xlukas15)
Date: 2025-03-30
-}

module Main where

import Data.List (isPrefixOf, minimumBy, sort, group)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import System.Environment (getArgs)

-- datova struktura pro rozhodovaci strom
data Tree = Node Int Double Tree Tree
    | Leaf String
    deriving (Show)

-- datova struktura pro radek souboru
data TreeLine = TreeNode Int (Int, Double)
    | TreeLeaf Int String
    deriving (Show)

-- ocekavany pocet mezer souboru pri zanoreni stromu
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

-- spocita uroven zanoreni
countIndent :: String -> Int
countIndent = length . takeWhile (== ' ')

-- zpracuje uzel
parseNode :: String -> (Int, Double)
parseNode line =
    case splitOn ", " (drop 6 (trimStart line)) of
        [idxStr, threshStr] -> (read idxStr, read threshStr)
        _ -> error "Neplatny format uzlu"

-- zpracuje list
parseLeaf :: String -> String
parseLeaf line = drop 6  (trimStart line)

-- zpracuje cely radek souboru
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
    let 
        (leftSubTree, rest1) = buildSubTree (indent + expectedIndentStep) rest
        (rightSubTree, rest2) = buildSubTree (indent + expectedIndentStep) rest1
    in 
        (Node index threshold leftSubTree rightSubTree, rest2)
buildTree (TreeLeaf _ label : rest) = (Leaf label, rest)
buildTree [] = error "Neocekavany konec souboru"

-- vytvari podstrom
buildSubTree :: Int -> [TreeLine] -> (Tree, [TreeLine])
buildSubTree _ [] = error "Neocekavany konec souboru - chybi potomek uzlu."
buildSubTree expectedIndent treeLines@(x:_) =
    case x of
        TreeNode indent _ ->
            if indent == expectedIndent then
                buildTree treeLines
            else error "Nespravna indentace uzlu."
        TreeLeaf indent _ ->
            if indent == expectedIndent then
                buildTree treeLines
            else error "Nespravna indentace listu."

-- =======================================
-- ========= DATA CLASSIFICATION =========
-- =======================================

-- proparsuje vstupni radek, rozdeli podle carek a pomoci map read premapuje String na Double
parseData :: String -> [Double]
parseData line = map read (splitOn "," (trimStart line))

-- udela klasifikaci dat ve strome
classifyData :: Tree -> [Double] -> String
classifyData (Leaf className) _ = className
classifyData (Node index threshold left right) features = 
    let 
        feature = features !! index
    in 
        if feature < threshold then
            classifyData left features
        else classifyData right features

-- =======================================
-- ============ TREE TRAINING ============
-- =======================================

-- zpracuje trenovaci data
parseTrainData :: String -> ([Double], String)
parseTrainData line =
    let 
        parts = splitOn "," (trimStart line)
        features = map read (init parts)
        label = last parts
    in 
        (features, label)

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

-- funkce pro vypocet skore pomoci gini indexu
calculateScore :: [([Double], String)] -> Int -> Double -> Double
calculateScore dataset featureIndex threshold =
    let
        -- rozdelim si dataset na dve poloviny podle thresholdu
        left = [(features, label) | (features, label) <- dataset, features !! featureIndex < threshold]
        right = [(features, label) | (features, label) <- dataset, features !! featureIndex >= threshold]

        numLeft = length left
        numRight = length right
        numComb = fromIntegral (numLeft + numRight)

        weightedGini =
            if numComb == 0 then -- pokud je dataset prazdny
                1.0 -- vratime nejhorsi skore
            else -- jinak vypocitame gini index
                (fromIntegral numLeft / numComb) * calculateGini (map snd left) +
                (fromIntegral numRight / numComb) * calculateGini (map snd right)
    in
        weightedGini

-- najde nejlepsi rozdelovaci bod v trenovacich datech
findBestSplit :: [([Double], String)] -> (Int, Double, Double)
findBestSplit dataset
    | null dataset = error "Empty dataset!"
    | length (group (sort (map snd dataset))) == 1 = error "Redundant split!"
    | otherwise = minimumBy (comparing (\(_, _, gini) -> gini)) allSplits -- ze vsech splitu se vybere ten nejmensi
    where
        numFeatures = length (fst (head dataset)) -- pocet priznaku prvniho prvku, potom predpokladame stejne
        allSplits = concat [findSplits dataset featureIndex | featureIndex <- [0..(numFeatures-1)]]

        -- (dataset, index_priznaku) -> [(feature_index, threshold, score)]
        -- najde a vypocita vsechny splity
        findSplits :: [([Double], String)] -> Int -> [(Int, Double, Double)]
        findSplits inputData featureIndex =
            let
                featureValues = [(features !! featureIndex, features, label) | (features, label) <- inputData] -- vezme vsechny trojice (ID, [feature], label)
                sortedByFeature = sort featureValues

                -- vypocitaji se mozne prahy
                -- vnitrni zip sjednoti sousedni vrcholy s podminkou, ze nebudou stejne
                -- vnejsi zip potom kazdemu paru vrcholu priradi index
                -- format: [(index, (vrchol1, vrchol2))]
                thresholds = [((v1 + v2) / 2, i)| 
                    (i, ((v1, _, _), (v2, _, _))) <- zip [0 :: Int ..] (zip sortedByFeature (tail sortedByFeature)),
                    v1 /= v2]
            in
                -- pro kazdy prah vypocita skore a vrati trojici (index priznaku, prah, skore)
                [(featureIndex, threshold, calculateScore inputData featureIndex threshold) |
                    (threshold, _) <- thresholds] -- vezme si pouze hodnotu thresholdu

-- natrenuje strom na zaklade trenovacich dat
trainTree :: [([Double], String)] -> Tree
trainTree dataset
    | null dataset = error "Empty dataset!"
    | length (group (sort labels)) == 1 = Leaf (head labels) -- data jsou v jedne tride => Leaf
    | otherwise = Node featureIndex threshold (trainTree leftDataset) (trainTree rightDataset) -- data nemaji stejnou tridu => Node
    where
        -- seznam trid
        labels = map snd dataset

        -- najdeme nejlepsi rozdeleni
        (featureIndex, threshold, _) = findBestSplit dataset
        
        leftDataset = [(features, label) | (features, label) <- dataset, features !! featureIndex < threshold]
        rightDataset = [(features, label) | (features, label) <- dataset, features !! featureIndex >= threshold]

-- funkce pro prevod stromu na vystupni format
treeToOutput :: Tree -> [String]
treeToOutput tree = treeToOutputHelper tree 0
    where
        treeToOutputHelper :: Tree -> Int -> [String]
        treeToOutputHelper (Leaf label) depth =
            [indent ++ "Leaf: " ++ label]
            where 
                indent = replicate (depth * expectedIndentStep) ' '
        treeToOutputHelper (Node featureIndex threshold left right) depth =
            [indent ++ "Node: " ++ show featureIndex ++ ", " ++ show threshold]
                ++ treeToOutputHelper left (depth + 1)
                ++ treeToOutputHelper right (depth + 1)
            where
                indent = replicate (depth * expectedIndentStep) ' '


-- ======================================
-- ================ MAIN ================
-- ======================================

-- vypise napovedu na vystup
showHelp :: IO ()
showHelp = putStrLn $ unlines
    [ "\nPouziti programu flp-fun"
    , "========================\n"
    , "flp-fun -1 <soubor obsahujici strom> <soubor_obsahujici nove data>"
    , "\t- Provede klasifikaci dat (soubor 2) na zaklade rozhodovaciho stromu (soubor 1).\n"
    , "flp-fun -2 <soubor obsahujici trenovaci data>"
    , "\t- Natrenuje rozhodovaci strom na zaklade trenovacich dat."
    ]

-- hlavni beh programu
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-1", treeFile, dataFile] -> do
            treeContent <- loadFile treeFile
            let treeLines = map parseLine treeContent
            let (tree, remainingLines) = buildTree treeLines
            if null remainingLines then
                do
                    dataContent <- loadFile dataFile
                    let parsedData = map parseData dataContent
                    let results = map (classifyData tree) parsedData
                    mapM_ putStrLn results
            else error "Neplatna struktura stromu - prebyvajici radky."
        ["-2", trainFile] -> do
            trainContent <- loadFile trainFile
            let trainData = map parseTrainData trainContent
            let tree = trainTree trainData
            let output = treeToOutput tree
            mapM_ putStrLn output
        ["-h"] -> showHelp
        ["--help"] -> showHelp
        _ -> putStrLn "Nepravny vstup. Pro zobrazeni napovedy pouzijte 'flp-fun -h' nebo 'flp-fun --help'."
