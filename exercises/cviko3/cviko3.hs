-- FUNCTOR
-- monada je typova trida s funkce ...
-- Functor ma funkce 'fmap' a '<$>', coz je to stejne
-- funktor existuje pro velke mnozstvi typovych trid

-- MONADS
-- '>>='
-- Just 3 >>= (\x -> Just $ x*5)
-- vraci 15

-- A uz jedeme kody ze ZKOUSEK

module F where

-- PRIKLAD
-- mame navrhnout datovy typ pro zasobnik nad obecnym datovym typem
-- nad nim funkce:
-- 'push' - vlozi prvek na zasobnik
-- 'pop' odstrani prvek ze zasobniku
-- 'top' vrati prvek na vrcholu zasobniku
-- 'len' vrati pocet prvku na zasobniku

-- Zasobnik [obsah] delka
data Stack a = Stack [a] Int deriving (Show)

push :: a -> Stack a -> Stack a
push top (Stack rest len) = Stack (top:rest) (len + 1)

pop :: Stack a -> (a, Stack a)
pop (Stack (top:rest) len) = (top, Stack rest (len - 1))

top :: Stack a -> a
top (Stack (top:_) _) = top

len :: Stack a -> Int
len (Stack _ len) = len

-- DALSI PRIKLAD
-- nadefinujte datovy typ pro double linked list
-- a funkci, co prevede seznam na double linked list
-- !!! nejsou povolene Maybe a neco podobneho !!!

data DLL a
    = EmptyDLL
    | DLL 
        { val :: a
        , prev :: DLL a
        , next :: DLL a
        } deriving (Show)

-- convert :: [a] -> Maybe (DLL a)
-- convert (x:xs) = DLL {val = x, prev = Nothing, next = arrToDll xs}
-- takze musim si sahat na vic prvku toho pole najednou stylem (x1:x2:xs)
convert :: [a] -> DLL a
convert [] = EmptyDLL

-- DALSI PRIKLAD
-- mejme datovy typ stromu s dvema konstruktory, naprogramujte dane funkce
data Tree k v
    = EmptyTree
    | Node k v (Tree k v) (Tree k v)
    deriving (Show)

treeFind :: (Ord k) => k -> Tree k v -> Maybe v
treeFind srch EmptyTree = Nothing
treeFind srch (Node k v left right)
    | srch == k = Just v
    | srch < k = treeFind srch left
    | otherwise = treeFind srch right

treeInsert :: (Ord k) => (k, v) -> Tree k v -> Tree k v
treeInsert (saveKey, saveVal) EmptyTree = Node saveKey saveVal EmptyTree EmptyTree
treeInsert (saveKey, saveVal) (Node k v left right)
    | saveKey == k = Node k v left right
    | saveKey < k = treeInsert (saveKey, saveVal) left
    | otherwise = treeInsert (saveKey, saveVal) right

-- dostane seznam prvku 'klic-hodnota' a udela z nich strom
treeCreateHelper :: (Ord k) => (k, v) -> Tree k v -> Tree k v
treeCreateHelper (sk, sv) EmptyTree = Node sk sv EmptyTree EmptyTree
treeCreateHelper (sk, sv) (Node k v left right)
    | sk == k = Node k sv left right
    | sk < k = Node k v (treeCreateHelper (sk, sv) left) right
    | otherwise = Node k v left (treeCreateHelper (sk, sv) right)

treeCreate :: (Ord k) => [(k, v)] -> Tree k v
treeCreate [] = EmptyTree
treeCreate ((k, v):xs) = treeCreateHelper (k, v) (treeCreate xs) -- tohle je hodne smart

-- ulozi in-order pruchodem stromu klice v zadanem BVS do seznamu
inord :: Tree k v -> [k]
inord EmptyTree = []
inord (Node k _ left right) = (inord left) ++ [k] ++ (inord right)

-- pridam si preorder a postorder
preord :: Tree k v -> [k]
preord EmptyTree = []
preord (Node k _ left right) = [k] ++ (preord left) ++ (preord right)

postord :: Tree k v -> [k]
postord EmptyTree = []
postord (Node k _ left right) = (postord left) ++ (postord right) ++ [k]

-- ulozi data (values) v zadanem BVS do seznamu tak, aby v nem nebyly duplikaty
createList :: Eq v => Tree k v -> [v]
createList EmptyTree = []
createList (Node _ v l r) = (createList l) ++ [v] ++ (createList r)

cleanList :: Eq v => [v] -> [v]
cleanList [] = []
cleanList (x:xs)
    | elem x xs = cleanList xs
    | otherwise = x : cleanList xs

inode :: Eq v => Tree k v -> [v]
inode EmptyTree = []
inode tree = cleanList (createList tree)
