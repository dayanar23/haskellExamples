data SuffixTree = Leaf Int 
                | Node [(String, SuffixTree)]
                deriving (Eq, Ord, Show)

t1 :: SuffixTree
t1 = Node [("banana", Leaf 0),
            ("a", Node [("na", Node [("na", Leaf 1),
                                    ("", Leaf 3)]),
                        ("", Leaf 5)]),
            ("na", Node [("na", Leaf 2),
                        ("", Leaf 4)])]

{-
    SUPPORT FUNCTIONS
-}

isPrefix :: String -> String -> Bool
isPrefix xs ys = length xs <= length ys && all (uncurry (==)) zs
    where zs = zip xs ys

isPrefix':: String -> String -> Bool
isPrefix' p s = length p <= length s &&
                let ps = zip p s in  all (uncurry(==)) ps

removePrefix :: String -> String -> String
removePrefix p s = if isPrefix p s 
                   then  drop (length(p)) s
                   else "error: " ++ p ++ "  no es prefijo de " ++ s --Fix for pure function 

suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes s = s : suffixes (tail s)

isSubstring :: String -> String -> Bool
isSubstring s1 s2 = any (isPrefix s1) (suffixes s2)

findSubstring:: String -> String -> [Int]
findSubstring s1 s2 = findInd (isPrefix s1) (suffixes s2)
  where findInd p xs = [ i | (x,i) <- zip xs [0..], p x]

{-
    SEARCHING OVER TREES
-}

getIndices :: SuffixTree -> [Int]
getIndices (Leaf t) = [t]
getIndices (Node tn) = concatMap (getIndices . snd) tn


findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' s (Leaf t) = []
findSubstrings' s (Node tn) = undefined
--          | selectStree s tn = getIndices  
--          |otherwise = [] 
--        where
countSubtrees:: SuffixTree -> Int
countSubtrees (Node t) = length t

showSubtrees:: SuffixTree -> [String]
showSubtrees  (Node t) = map fst t

selectStree:: SuffixTree -> String -> Bool
selectStree t s = any (isPrefix s) (showSubtrees t) 

showTree:: SuffixTree ->[String]
--showTree (Leaf t) = []
showTree (Leaf t) = []
showTree (Node (x:xs)) = fst x : map (showTree . snd) xs


showAllTree:: SuffixTree -> [String]
showAllTree tn = undefined













{-
    BUILDING SUFFIX TREES
-}
