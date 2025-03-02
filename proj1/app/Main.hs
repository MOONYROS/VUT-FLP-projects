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

-- nalezeni nejlepsiho rozdeleni datasetu, vraci (index priznaku, prahovou hodnotu, skore rozdeleni)
findBestSplit :: [([Double], String)] -> (Int, Double, Double)
findBestSplit dataset
    -- pro prazdny dataset vracime vychozi hodnoty
    | null dataset = (-1, 0.0, 1.0) 
    -- pokud vsechny prvky patri do stejne tridy, neni potreba nic delit
    | length (group (sort (map snd dataset))) == 1 = (-1, 0.0, 1.0)
    -- vybeteme rozdeleni s nejmensim skore
    | otherwise = minimumBy (comparing (\(_, _, score) -> score)) allSplits
    where
        -- pocet priznaku - bereme z prvniho prvku a u zbytku predpokladame stejny pocet
        numFeatures = length (fst (head dataset))
        
        -- vygenerujeme pro vsechny priznaky vsechna mozna rozdeleni
        allSplits = concat [findSplitsForFeature dataset featureIndex | featureIndex <- [0..(numFeatures-1)]]
    
        -- vnorena funkce pro hledani rozdeleni pro konkretni priznak
        -- vstupem je (dataset, index priznaku)
        findSplitsForFeature :: [([Double], String)] -> Int -> [(Int, Double, Double)]
        findSplitsForFeature inputData featureIndex =
            let 
                -- seradime data podle hodnoty konkretniho priznaku
                featureValues = [(features !! featureIndex, features, label) | (features, label) <- inputData]
                sortedByFeature = sort featureValues

                -- vygenerujeme mozne prahy jako prumery sousednich hodnot
                thresholds = [((v1 + v2) / 2, i) |
                    (i, ((v1, _, _), (v2, _, _))) <- zip [0 :: Int ..] (zip sortedByFeature (tail sortedByFeature)),
                    v1 /= v2]
            in 
                -- pro kazdy prah vypocitam skore a vratim trojici (index priznaku, prah, skore)
                [(featureIndex, threshold, calculateSplitScore inputData featureIndex threshold) |
                    (threshold, _) <- thresholds]

-- funkce pro vypocet skore rozdeleni - nizsi znamena lepsi
calculateSplitScore :: [([Double], String)] -> Int -> Double -> Double
calculateSplitScore dataset featureIndex threshold =
    let
        -- rozdelim dataset na levou a pravou cast podle prahu
        left = [(features, label) | (features, label) <- dataset, features !! featureIndex <= threshold]
        right = [(features, label) | (features, label) <- dataset, features !! featureIndex > threshold]
        
        -- spocitam velikosti obou casti (stran)
        nLeft = length left
        nRight = length right
        n = fromIntegral (nLeft + nRight)
        
        -- vypocitam vazeny gini index - vahy odpovidaji velikosti kazde casti
        weightedGini =
            if n == 0
                then 1.0 -- pokud je dataset prazdny, vratime nejhorsi skore
                else (fromIntegral nLeft / n) * calculateGini (map snd left) + 
                    (fromIntegral nRight / n) * calculateGini (map snd right)
    in
        weightedGini

-- hlavni funkce pro vybudovani rozhodovaciho stromu
-- maxDepth = maximalni hloubka stromu
-- minSamples = minimalni pocet vzorku pro dalsi deleni
-- currentDepth = aktualni hloubka pro rekurzi
buildCartTree :: [([Double], String)] -> Int -> Int -> Int -> Tree
buildCartTree dataset maxDepth minSamples currentDepth
    | null dataset = Leaf "Unknown"  -- pokud je dataset prazdny, vratime vychozi list
    | length dataset < minSamples = Leaf (mostCommonLabel dataset)  -- prilis malo vzorku => vytvorime list
    | currentDepth >= maxDepth = Leaf (mostCommonLabel dataset)  -- dosazeni maximalni hloubky => vytvorime list
    | length (group (sort labels)) == 1 = Leaf (head labels)  -- vsechny vzorky maji stejnou tridu => vytvorime list
    | featureIndex == -1 = Leaf (mostCommonLabel dataset)  -- nelze najit vhodne rozdeleni => vytvorime list
    | otherwise = Node featureIndex threshold -- vytvorime vnitrni uzel s rekurzivnimi podstromy
                    (buildCartTree leftDataset maxDepth minSamples (currentDepth + 1)) -- levy podstrom
                    (buildCartTree rightDataset maxDepth minSamples (currentDepth + 1)) -- pravy podstrom
    where
        -- seznam trid
        labels = map snd dataset
        
        -- najdeme nejlepsi rozdeleni
        (featureIndex, threshold, _) = findBestSplit dataset
        
        -- rozdelime dataset podle nejlepsiho rozdeleni
        leftDataset = [(features, label) | (features, label) <- dataset, features !! featureIndex <= threshold]
        rightDataset = [(features, label) | (features, label) <- dataset, features !! featureIndex > threshold]

-- funkce pro urceni nejcastejsi tridy v datasetu - pro vytvoreni listu, kdyz nechceme dale delit
mostCommonLabel :: [([Double], String)] -> String
mostCommonLabel dataset =
    -- vytvorim mapu cetnosti trid a vyberu tridu s nejvyssi cetnosti
    fst $ maximumBy (comparing snd) $ Map.toList $ Map.fromListWith (+) [(label, 1 :: Int) | (_, label) <- dataset]

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
