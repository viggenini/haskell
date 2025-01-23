import Data.List
import Data.Char (chr, ord, isAlpha, isAlphaNum, isDigit, digitToInt,)
import Data.Text.Internal.Fusion.Size (larger)
import Distribution.Compat.Lens (_1)
import Data.Time

--------------------------- Kapitel 1 -------------------------------------------------------------------------------------------------------------------------
--Övningar--
--3 
-- Definera en funktion som multiplicerar alla element i en lista
productEx :: [Int] -> Int
productEx [] = 0
productEx xs = product xs

--4 Ändra definition av qsort så att den sorterar större till mindre istället för mindre till större
-- Byt plats på qsort smaller ++ [x] ++ qsort larger
qsort [] =[]
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
                where
                    smaller = [a | a <- xs, a <= x]
                    larger  = [b | b <- xs, b >  x]

-- 5
-- Vad händer om man byter <= mot < i den första versionen av qsort?
-- qsort tar bort dubletter när den sorterar listan. qsort [2,2,3,1,1] = [1,2,3]

--------------------------- Kapitel 2 --------------------------------------------------------------------------------------------------------------------------

--------------------------- Kapitel 3 --------------------------------------------------------------------------------------------------------------------------

--------------------------- Kapitel 4 --------------------------------------------------------------------------------------------------------------------------
--Exempel från kapitlet:

-- alla udda tal upp till n
odds :: Int -> [Int]
odds n = map (\x -> x*2 + 1) [0..n-1]
-- Funktion som ger const a b = a alltid
const :: a -> (b -> a)
const x = \_ -> x

{-
Exempel på funktioner i lambda form:
1. (+)  ->  \x -> (\y -> x + y)
2. (1+) ->  \x -> 1 + x
3. (1/) ->  \x -> 1 / x
4. (*2) ->  \x -> x * 2
5. (/2) ->  \x -> x / 2
-}

-- Övningar --
-- 1
-- Definera en funktion som delar en lista med jämnt antal element på mitten
halve :: [a] -> ([a],[a])
halve list = splitAt ((length list) `div` 2) list

-- 2
-- Definera en funktion som returnerar det 3:e elementet i en lista med minst så många element genom att använda:
-- a) Head och tail
third1 :: [a] -> a
third1 list = head (tail (reverse list))

-- b) List indexering !!
third2 :: [a] -> a
third2 list = list !! 1

-- c) Pattern matching
third3 :: [a] -> a
third3 [a,b,c] = b
third3 _ = error("ooba")

-- 3
-- Definera en funktion safeTail som inte producerar ett fel när en tom lista används genom att använda:
-- a) Conditional expression
safeTail1 :: [a] -> [a]
safeTail1 xs = if length xs == 0 then [] else tail xs

-- b) Guards
safeTail2 :: [a] -> [a]
safeTail2 xs
    |null xs    = []
    |otherwise  = tail xs

-- c) Pattern matching
safeTail3 :: [a] -> [a]
safeTail3 [] = []
safeTail3 xs = tail xs

-- 4
-- Visa på 4 olika sätt hur || eller OR kan defineras med pattern matching

or1 :: Bool -> Bool -> Bool
or1 a False = a
or1 True _ = True
or1 _ True = True

-- 7
{-
Skriv om följande kod så att den använder lambda uttryck:
mult :: Int -> Int -> Int -> Int
mult x y z = x * y * z
-}
mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x * y * z))

luhnDouble :: Int -> Int
luhnDouble = \x ->  if x * 2 > 9 then (x * 2) - 9 else x * 2

luhn1 :: String -> String
luhn1 digits = map verify digits
        where
            verify c
                |isDigit c = chr $ luhnDouble (ord c - ord '0') + ord '0'
                |otherwise = error"Only numbers are valid inputs."

isValidLuhn1 :: String -> Bool
isValidLuhn1 digits = sum (map digitToInt digits) `mod` 10 == 0

luhn2 :: Int -> Int -> Int -> Int -> Bool
luhn2 x y z w = mod (luhnDouble x + (if y > 9 then y - 9 else y) + luhnDouble z + (if w > 9 then w - 9 else w)) 10 == 0 

luhn :: IO()
luhn = do
    putStrLn ("Input number to be verified:")
    number <- getLine
    putStrLn ("Veryfing.." ++ "\n")
    let transformedDigits = luhn1 number
    if isValidLuhn1 transformedDigits 
        then putStrLn ("Valid.")
        else putStrLn ("Invalid.")                 
                    
--------------------------- Kapitel 5 - List Comprehension ------------------------------------------------------------------------------------------------------------
powerOf2 :: (Num a, Enum a) => a -> [a]
powerOf2 n = [x^2 | x <- [1..n]]

-- Exempel med guards --
-- allPairs ger alla kombinationer av 2 listor --
allPairs :: [a] -> [b] -> [(a,b)]
allPairs xs ys = [(x,y) | x <- xs, y <- ys]

allPairs2 :: [a] -> [b] -> [(a,b)]
allPairs2 xs ys = [(x,y) | y <- ys, x <- xs]

-- Primtal --
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [1..n], prime x == True ]

-- find --
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

-- Övningar --
-- 1 --
--Definera en funktion som ger summan av 1^2..100^2
sumSqr :: Int -> Int
sumSqr n = sum [x^2 | x <- [1..n]]

-- 2 --
-- Definera en funktion grid som ger alla koordinater i ett grid
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- 3 --
-- Definera en square grid funktion
sqrGrid :: Int -> [(Int, Int)]
sqrGrid n = [(x,y) | x <- [0..n], y <- [0..n], x /= y]

-- 4 --
-- Definera en egen version av replicate
myReplicate :: Int -> a -> [a]
myReplicate n xs = [xs | _ <- [1..n]]

-- 5 --
-- Definera en funktion som returnerar pythagoras triplets upp till ett maxvärde
pythagoras :: Int -> [(Int, Int, Int)]
pythagoras n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], (a^2) + (b^2) == (c^2)]

-- 6 --
-- Definera en funktion som returnerar alla perfekta tal upp till n
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) == x + x]


--------------------------- Kapitel 6 - Rekursiva funktioner -----------------------------------------------------------------------------------------------------
-- Exempel från kapitlet --

-- Factorial --
fac :: Int -> Int
fac n   
    | n <  0    = error"No negative numbers allowed"
    | n == 0    = 1
    | otherwise = n * fac (n-1)

readPost :: IO (String, String, Int)
readPost = do
    putStrLn"What is your name?"
    name <- getLine
    putStrLn"Please write your messsage:"
    msg <- getLine
    putStrLn"Who can see the post? (0 = private, 1 = friends, 2 = public)"
    vis <- readLn
    if vis >= 0 && vis <= 2
        then return (name, msg, vis)
        else error"invalid input"

 

-- Definera en rekursiv funktion med 5 steg --
-- Exempel 1 : product
{- 2: Lägg till fallen
product1 :: [Int] -> Int
-}

{- 2: Lägg till olika fall som behövs
product1 []     =
product1 (n:ns) =
-}

{- 3: Definera dom enkla fallen
product1 []     = 1
product1 (n:ns) =
-}

{- 4: Definera dom andra fallen
product1 []     = 1
product1 (n:ns) = n * product1 ns
-}

{- 5: Generalisera och förenkla
Vi kan använda en annan typ för funktionen:
product1 :: Num a => [a] -> a

Vi kan förenkla hela funktionen till 1 rad genom att använda foldr: -}
product1 :: Num a => [a] -> a
product1 = foldr (*) 1

-- Exempel 2 : drop
drop1 :: Integral b => b -> [a] -> [a]
drop1 0 []      = []
drop1 0 (x:xs)  = x:xs
drop1 n []      = []
drop1 n (x:xs)  = drop1 (n-1) xs

-- Övningsuppgifter --
-- 1: Modifiera fac så att negativa tal inte resulterar i stack overflow (se fac i början av kap 6 ovan)

-- 2: Definera sumDown :: Int -> Int som summerar alla tal från n ner till 0
sumDown :: Int -> Int
sumDown 0 = 0
sumDown 1 = 1
sumDown n = sumDown (n-1) + n

-- 3: Definera ^ operator rekursivt på samma sätt som product1 *
power :: Int -> Int -> Int
power _ 0 = 1
power 1 _ = 1
power n k = n * power n (k-1)

-- 4: Definera en rekursiv funktion euclid :: Int -> Int -> Int som implementerar Euklides algoritm för GCD
euclid :: Int -> Int -> Int
euclid 0 0  = 0
euclid a 0  = abs a
euclid _ 1  = 1
euclid a b  = euclid b (a `mod` b)
-- 6: Definera följande library funktioner rekursivt:
-- a) and :: [Bool] -> Bool
and1 :: [Bool] -> Bool
and1 []         = error"Empty list"
and1 [True]     = True
and1 [False]    = False
and1 (x:xs)     = and1 xs

-- b) concat :: [[a]] -> [a]
concat1 :: [[a]] -> [a]
concat1 []    = []
concat1 [[]]  = []
concat1 (x:xs)  = x ++ concat1 xs 

-- c: replicate :: Int -> a -> [a]
replicate1 :: Int -> a -> [a]
replicate1 0 a  = []
replicate1 1 a  = [a]
replicate1 n a   = a : replicate1 (n-1) a

-- d: (!!) :: [a] -> Int -> a
index1 :: [a] -> Int -> a
index1 [] _     = error"Empty list"
index1 (x:xs) 0 = x
index1 (x:xs) 1 = head xs
index1 (x:xs) n = index1 xs (n-1)

-- e: elem :: Eq a => a -> [a] -> Bool
elem1 :: Eq a => a -> [a] -> Bool
elem1 n []      = False
elem1 n (x:xs)  
            |n == x         = True
            |null xs        = False
            |n == head xs   = True
            |otherwise      = elem1 n xs

-- 7: Definera merge :: Ord a => [a] -> [a] -> [a] som sorterar 2 listor och mergar dom
merge :: Ord a => [a] -> [a] -> [a]
merge [] []         = []
merge [] ys         = ys
merge xs []         = xs
merge (x:xs) (y:ys) 
    |x < y      = x : merge xs (y:ys)
    |y < x      = y : merge (x:xs) ys
    |otherwise  = x : merge xs ys


    
--------------------------- Kapitel 7 - Higher-order funktioner ------------------------------------------------------------------------------------------------------------
-- Exempel från kapitlet --
{- 
Exempel på rekursiv struktur på funktioner som har med listor att göra:

f []     = v
f (x:xs) = x # f xs
Applicerar operatorn # på head (x:xs) och sedan rekurivt genom head xs osv

sum []      = 0
sum (x:xs)  = x + sum xs

product []      = 1
product (x:xs)  = x * product xs

Genom att använda foldr kan vi skriva om funktionerna ovan:

f :: [a] -> a -> (a -> a -> a) -> a
f = foldr (#) v

sum :: Num a => [a] -> a
sum = foldr (+) 0

product :: Num a => [a] -> a
product = foldr (*) 1

Definition av foldr:
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v []        = v
foldr f v (x:xs)    = f x (foldr f v xs)


-}
-- Binary string transmitter
type Bit = Int -- Deklarera en typ som kan vara användbar
bin2int1 :: [Bit] -> Int
bin2int1 bits = sum [w*b| (w,b) <- zip weights bits]
                where weights = iterate (*2) 1

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0   = []
int2bin n   = n `mod` 2 : int2bin (n `div` 2) 

make8bit1 :: [Bit] -> [Bit]
make8bit1 []     = [0,0,0,0,0,0,0,0]
make8bit1 bits
        |length bits  < 8     = make8bit1 (0 : bits)
        |length bits == 8     = bits
        |length bits >  8     = error"Number of bits greater than 8."
        |otherwise = make8bit1 bits

make8bit :: [Bit] -> [Bit]
make8bit bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concatMap (make8bit . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

channel :: [Bit] -> [Bit]
channel = id

transmitter :: String -> String
transmitter = decode . channel . encode

transmit :: IO()
transmit = do
    putStrLn "Enter message.."
    msg <- getLine

    putStrLn "Encoding..\n"
    let encodedMsg = encode msg
    putStrLn $ "Encoded message: " ++ show encodedMsg

    putStrLn "Decode? y/n"
    reply <- getLine
    if reply == "y"
        then putStrLn $ "Decoded message: " ++ decode encodedMsg
        else putStrLn "Exiting.."


--------------------------- Kapitel 8 - Data typer ------------------------------------------------------------------------------------------------------------
-- Exempel från kapitlet --
-- Rekursiva typer : Träd --
{-
data Tree a = Leaf a | Node (Tree a) a (Tree a) 
t :: Tree Int
t = Node (Node (Leaf 2) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

mirror1 :: Tree Int -> Tree Int
mirror1 (Leaf a)             = Leaf a
mirror1 (Node left b right)  = Node (mirror1 right) b (mirror1 left)

root1 :: Tree Int -> Int
root1 (Node left b right) = b

mirrorprop :: Eq (Tree Int) => Tree Int -> Bool
mirrorprop tree = tree == mirror1(mirror1 tree)

rootprop :: Eq (Tree Int) => Tree Int -> Bool
rootprop tree
    |root1 tree == root1 (mirror1 tree) = True
    |otherwise = False



instance Eq a => Eq (Tree a) where
    (Leaf x) == (Leaf y) = x == y
    (Node l1 v1 r1) == (Node l2 v2 r2) = l1 == l2 && v1 == v2 && r1 == r2
    _ == _ = False

instance Show a => Show (Tree a) where
    show (Leaf a) = "Leaf " ++ show a
    show (Node left b right) = "Node (" ++ show left ++ ") " ++ show b ++ " (" ++ show right ++ ")"

--(Node (_) _ (_)) d (Node (_) _ (_))  = (Node (a) b (c)) d (Node (e) f (g))

-}

{- Visualisering av t:
     5
   /  \
  3    7
 / \  / \
2  4 6   9
-}
--------------------------- Övningsuppgifter från gamla tentor -------------------------------------------------------------------------------------------------------------
interleave :: [a] -> [a] -> [a]
interleave [] [] = []
interleave xs [] = xs
interleave [] ys = ys
interleave (x:xs) (y:ys) = x : y : interleave xs ys


fun [] = 10
fun [x] = x
fun (x:xs) = 1 + fun xs

f [x] = x
f (x:xs) = g x (f xs)
    where
        g x y   | x >= y = x
                | otherwise = y

remove :: Int -> [Int] -> [Int]
remove n xs = [x |x <- xs, x /= n]

removeDups :: [Int] -> [Int]
removeDups [] = []
removeDups (x:xs) 
            |x `elem` xs    = x : removeDups(remove x xs)
            |otherwise      = x : removeDups xs
            
-- Omtenta aug 2024 --
-- 1:
{-
ack 0 n = n + 1
ack m 0 = ack (m-1) 1
ack m n = ack (m-1) (ack m (n-1) )

-- a) ack 1 1 =

ack 1 1 =
    ack (1-1) (ack 1 (1-1)) 
    = ack 0 (ack 1 (0))
    = (ack 1 (0)) + 1
    = ack (1-1) 0 + 1
    = ack 0 0     + 1
    = 0 + 1 + 1 
    = 2
-- b) type signature of ack
ack :: Int -> Int -> Int

-}

-- 2: Implement findIndex :: Eq a => a -> [a] -> Maybe Int:
findIndex1 :: Eq a => a -> [a] -> Maybe Int
findIndex1 _ []    = Nothing
findIndex1 y (x:xs) 
    | y == x    = Just 0
    | otherwise = case findIndex1 y xs of
                    Nothing -> Nothing
                    Just idx -> Just (idx + 1)

-- [a,b,c,d]
-- [(0,a), (1,b), (2,c), (3,d)]
--[x | x <- fst zip[0..] list
prop_element :: Int -> [Int] -> Bool
prop_element i xs =
    case findIndex1 i xs of
        Nothing -> i `notElem` xs
        Just idx -> (xs !! idx) == i

-- 3: Implementera Y!
data Ys = Y 
    { users  :: [User]
    , tweets :: [Tweet]
    } deriving (Show, Read)

data User = User
    { realName :: String
    , nickName :: String
    } deriving (Show, Read)

data Tweet = Tweet
    { author    :: User
    , content   :: String
    , date      :: String
    , language  :: Language 
    } deriving (Show, Read)

data Language = English | Swedish | Dutch
    deriving (Show, Read, Eq)



-- readDate reads the current date and time
readDate :: IO String
readDate = do
    currentTime <- getCurrentTime
    let formattedDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" currentTime
    return formattedDate


readTweet :: IO (String, String, String)
readTweet = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn "Please enter your message:"
    msg <- getLine
    date <- readDate
    return (name, msg, date)

data Color = Red | White | Blue deriving (Eq, Show)

containsFlag :: [Color] -> Bool
containsflag []                 = False
containsFlag [_,_]              = False
containsFlag [Red, White, Blue] = True
containsFlag [Red, White, Blue,_] = True
containsFlag (_:cs) = containsFlag cs

allSets :: [Int] -> [[Int]]
allSets []     = [[]]
allSets [x]    = [[] , [x]]
allSets (x:xs) = allSets xs ++ map (x:) (allSets xs)

sumSeqs :: Int -> [Int] -> [[Int]]
sumSeqs n xs = filter ((==n) . sum) (allSets xs)

getRole :: String -> IO String
getRole _ = return "Admin"

readUser :: IO (String, String)
readUser = do
    putStrLn "Please give your user name:"
    name <- getLine
    role <- getRole name
    return (name, role)


-- ghci> allSets [1,2,3]
-- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]            

-- [(a,b) | a <-[0..(length (x:xs) - 1)], b <- (x:xs)]
{-
Notes: Viktiga koncept för Del I av tentan
- write a recursive function over a list

- write a recursive function over a number

- write a function that needs to use guards

- use a standard higher-order function such as map, filter, or takeWhile to solve a given problem

- write a (recursive) function over a given recursive datatype

- design your own (recursive) datatype

- simplify the definition of a function that does too much pattern matching, unnecessary case distinctions, has complicated Boolean expressions, etc.

- write a property for a given function

- write a function that does some simple IO (reading/writing files, printing on the screen, user input)

-}