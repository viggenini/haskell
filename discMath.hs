import Data.List (intercalate)

-- Kapitel 2: Mängdlära --
-- Mängdoperatorer --
-- A snitt B --
snitt :: Eq a => [a] -> [a] -> [a]
snitt [] []     = []
snitt _  []     = []
snitt (a:as) bs 
    |a `elem` bs = a : snitt as bs
    |otherwise   = snitt as bs

-- A union B --
unionSet :: [a] -> [a] -> [a]
unionSet [] []      = []
unionSet _  []      = []
unionSet xs ys      = xs ++ ys

-- Mängddifferens A / B --
setDiff :: Eq a => [a] -> [a] -> [a]
setDiff [] []       = []
setDiff xs []       = xs
setDiff [] ys       = ys
setDiff xs ys       = [z | z <- xs, not (z `elem` ys)]

-- Symmetrisk mängdifferen av A B = (A/B) U ( B/A)
symSetDiff :: Eq a => [a] -> [a] -> [a]
symSetDiff xs ys    = unionSet (setDiff xs ys) (setDiff ys xs)



-- :Kartesisk produkt A X B där A = [1,2], N = [a,b] -> A X B = [(1,a), (2,a), (1,b), (2,b)]
cartProd :: [a] -> [b] -> [(a,b)]
cartProd [] []      = []
cartProd xs ys      = [(a,b) | a <- xs, b <- ys]

-- :Potensmängden p(a) 
powerSet :: [a] -> [[a]]
powerSet []     = [[]]
powerSet [x]    = [[] , [x]]
powerSet (x:xs) = powerSet xs ++ map (x:) (powerSet xs)

-- Kapitel 6: Kombinatorik --

fac :: Int -> Int
fac n   
    | n <  0    = error"No negative numbers allowed"
    | n == 0    = 1
    | otherwise = n * fac (n-1)

nOverK :: Int -> Int -> Int 
nOverK n k = div (fac n) (fac k * fac (n - k))

nOverKplus :: Int -> Int -> Int
nOverKplus n k  = nOverK n k + nOverK n (k+1)


pascal :: Int -> [Int]
pascal 1        = [1]
pascal 2        = [1,1]
pascal n        = [1] ++ zipWith (+) prevRow (tail prevRow) ++ [1]
                        where 
                            prevRow = pascal (n-1)

pascalRow :: Int -> [Int]
pascalRow = pascal

-- Print rows with proper indentation
printRows :: Int -> String
printRows n = concatMap (\i -> printSpaces (n - i) ++ intercalate "  " (map show (pascalRow i)) ++ "\n") [1..n]


printTest :: IO()
printTest = do
    putStrLn (printRows 3)

printPascal :: IO()
printPascal = do
    putStrLn"How many rows would you like to print?"
    input <- getLine
    let n = read input :: Int
    putStrLn( printRows n)

printSpaces :: Int -> String
printSpaces n = replicate (2 * n) ' '     


-- Övningsuppgifter --
-- 6.1: Hur många möjliga utfall finns det vid kast av 3 olika tärningar?
-- 6 * 6 * 6

-- 6.2: Definera en funktion som beräknar hur många ord man kan bilda av en string
wordPermutations :: String -> Int
wordPermutations []     = 1
wordPermutations (x:xs) = length (x:xs) * wordPermutations xs

gcd1 :: Int -> Int -> Int
gcd1 x 0     = x
gcd1 x y     = gcd1 y (mod x y)

coprime :: Int -> Int -> Bool
coprime x y 
    |gcd1 x y == 1  = True
    |otherwise      = False