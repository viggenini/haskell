import Data.List
import Data.Char
import Data.Text.Internal.Fusion.Size (larger)
import Distribution.Compat.Lens (_1)
import Data.Time
import Distribution.Simple.Utils (xargs)


--------------------------- Kapitel 1 -------------------------------------------------------------------------------------------------------------------------
--Övningar--
--3 
-- Definera en funktion som multiplicerar alla element i en lista
productEx :: [Int] -> Int
productEx [] = 0
productEx xs = product xs



--------------------------- Kapitel 2 --------------------------------------------------------------------------------------------------------------------------
--  Exempel --

-- Standard prelude - listor ---------------------------------------------

-- Välj första elementet i en lista --
-- > head [1,2,3,4,5]
-- 1

-- Ta bort första elementet i en lista --
-- > tail [1,2,3,4,5]
-- [2,3,4,5]

-- Välj element n ur en lista ( n = 2) --
-- > [1,2,3,4] !! 2
-- 3

-- Välj de första n elementen ur en lista --
-- > take 3 [1,2,3,4,5]
-- [1,2,3]

-- Ta bort de första n elementen ur en lista --
-- > drop 2 [1,2,3,4,5]
-- [3,4,5]

-- Beräkna längden av en lista --
-- > length [1,2,3,4,5]
-- 5

-- Summera en lista --
-- > sum [1,2,3,4,5]
-- 15

-- Beräkna produkten av en lista --
-- > product [1,2,3,4,5]
-- 120

-- Lägg ihop listor --
-- > [1,2,3] ++ [4,5]
-- [1,2,3,4,5]

-- Reversera en lista --
-- > reverse [1,2,3,4,5]
-- [5,4,3,2,1]

-- Layout regler ---------------------------------------------------------

-- lowercase på funktioner
-- tab sensitive

exampleFunction = b + c
    where
        b = 1 
        c = 2
differentFunction = exampleFunction * 2


-- Övningar --------------------------------------------------------------

-- 3: Hitta 3 syntaxfel --------------------
-- N = a `div` length xs
--      where
--          a = 10
--         xs = [1,2,3,4,5]

-- Korrekt kod --
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

-- 4: Definera last ------------------------

last1 xs = drop ( length xs - 1) xs

last2 xs = xs !! (length xs - 1)

last3 xs = head (reverse xs)

-- 5: Definera init på 2 olika sätt -------- [ expression | pattern <- list, condition ]

init1 xs = reverse (tail (reverse xs))

init2 xs = take (length xs - 1) xs


--------------------------- Kapitel 3 --------------------------------------------------------------------------------------------------------------------------
-- Exempel --

-- Basic typer -----------------------------------------------------------

-- Bool: Logiska värden, true false etc
-- Char: Enstaka bokstäver, siffror
-- String: en lista med Chars, String = [Char]
-- Int: Numeriskt värde inom en viss range som jag aldrig kommer jobba utanför
-- Integer: Använd när man ska jobba utanför rangen ovan
-- Float: Single precision floating number, till exempel 1.0 eller 3.1415927
-- Double: Liknar float men med dubbelt så mycket minne allokerat, ökar precisionen

-- List typer ------------------------------------------------------------

-- [False, True, False]    :: [Bool]
-- ['a', 'b', 'c']         :: [Char]  
-- ["One", "Two", "Three"] :: [String]

-- Tuple typer -----------------------------------------------------------

-- (False, True)       :: (Bool, Bool)
-- (False, 'a', True)  :: (Bool, Char, Bool)
-- ("Yes", True, 'a')  :: (String, Bool, Char)

-- Går att ha mer komplicerade tuplar också --

-- ('a', (False, 'b'))         :: (Char, (Bool, Char))
-- (['a', 'b'], [False, True]) :: ([Char], [Bool])
-- [('a', False), ['b', True]] :: [(Char, Bool)]

-- Funktionstyper -------------------------------------------------------

-- not :: Bool -> Bool
-- even :: Int -> Bool

add :: (Int, Int) -> Int
add (x,y) = x + y

zeroto :: Int -> [Int]
zeroto n = [0..n]

-- zeroto med odd/even villkor -------------
ztodd :: Int -> [Int]
ztodd n = [x | x <- [0..n], odd x]

zteven :: Int -> [Int]
zteven n = [x | x <- [0..n], even x]

-- Egen övning -----------------------------
evenodd :: [Int] -> [[Int]]
evenodd xs = [[x | x <- xs, even x], [y | y <- xs, odd y]]

-- Polymorphic typer -----------------------------------------------------

-- length :: [a] -> Int
-- Låter listan vars length ska beräknas bestå av vilken typ av element som helst

-- Andra exempel --

-- fst  :: (a,b) -> a
-- head :: [a] -> a
-- take :: Int -> [a] -> [a]
-- zip  :: [a] -> [b] -> [(a,b)]

-- Overloaded typer ------------------------------------------------------

-- (+)      :: Num a => a -> a -> a
-- (*)      :: Num a => a -> a -> a
-- negate   :: Num a => a -> a
-- abs      :: Num a => a -> a

-- Övningar --------------------------------------------------------------

-- 2: Definera funktioner med olika typer

-- 2 a)

--------------------------- Kapitel 4 --------------------------------------------------------------------------------------------------------------------------

-- Conditional expressions -----------------------------------------------

abs1 :: Int -> Int
abs1 n = if n >= 0 then n else -n

-- Nested --

--signum1 :: Int -> Int
--signum1 n = if n < 0 then -1 else
--            if n == 0 then 0 else 1

-- Guards ----------------------------------------------------------------

-- Guards kan göra conditions lättare att skriva och läsa
-- Här är de tidigare funktionerna uttryckta med guards

abs2 :: Int -> Int
abs2 n  |n >= 0     = n
        |otherwise  = -n

signum2 :: Int -> Int
signum2 n   | n < 0     = -1
            | n == 0    = 0
            | otherwise = 1

-- Pattern matching ------------------------------------------------------

not1 :: Bool -> Bool
not1 False  = True
not1 True   = False

and1 :: Bool -> Bool -> Bool
and1 True True  = True
and1 _ _        = False

-- Mer optimerad pattern matching --

and2:: Bool -> Bool -> Bool
and2 True b     = b
and2 False _    = False

-- Tuple patterns --------------------------

fst1 :: (a,b) -> a
fst1 (x,_)  = x

snd1 :: (a,b) -> b
snd1 (_,y)  = y

-- List patterns ---------------------------

test :: [Char] -> Bool
test ['a', _, _]    = True
test _              = False

-- : (constructor operator) bygger ihop listor 
-- [1,2,3] = 1:(2:(3:[]))
-- Operatorn associerar till höger, så onödiga paranteser behöver inte skrivas
-- 1:(2:(3:[])) = 1:2:3:[] till exempel

-- Vi kan optimera test lite med cons 

test1 :: [Char] -> Bool
test1 ('a':_)   = True
test1 _         = False

-- Library funktionerna head och tail --

head1 :: [a] -> a
head1 (x:_)  = x

tail1 :: [a] -> [a]
tail1 (_:xs)    = xs

-- Övningar --------------------------------------------------------------

-- 1: Definera halve :: [a] -> ([a], [a])

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = (take (div (length xs) 2) xs, drop (div (length xs) 2) xs)

-- 2: Definera third :: [a] -> a 
-- som returnerar tredje elementet 
-- i en lista med minst 3 element

-- a) med head & tail

thirdA :: [a] -> a
thirdA xs = head (tail (tail xs))

-- b) med !!
thirdB :: [a] -> a
thirdB xs = xs !! 2

-- c) med pattern matching
thirdC :: [a] -> a
thirdC [_,_,x] = x


-- 3: Definera safetail :: [a] -> [a] 

-- a) med ett conditional uttryck

safetailA :: [a] -> [a]
safetailA xs = if null xs then [] else tail xs

-- b) med guards
safetailB :: [a] -> [a]
safetailB xs    | null xs       = []
                | otherwise     = tail xs

-- c) med pattern matching
safetailC :: [a] -> [a]
safetailC []    = []
safetailC xs    = tail xs

-- 4: Visa hur || kan defineras på ett liknande sätt som && med pattern matching

or :: Bool -> Bool -> Bool
or True False   = True
or False True   = True
or _ _          = False

-- 8: Luhn
luhnDouble :: Int -> Int 
luhnDouble n | n <= 4       = n * 2
             | otherwise    = (n * 2) - 9

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d | mod (d + luhnDouble c + b + luhnDouble a) 10 == 0    = True
             | otherwise                                            = False

--------------------------- Kapitel 5 --------------------------------------------------------------------------------------------------------------------------
-- List comprehension --
-- [x | x <- [1..n], condition]

-- Fler conditions: --
-- [ (x,y,z) | x <- [1..n], y [1..n], z <- [1..n]]

-- Ordered pairs : --
-- [(x,y) | x <- [1..n], y <- [x..n]]

concat1 :: [[a]] -> [a]
concat1 xss = [x | xs <- xss, x <- xs]

-- zip & andra mer avancerade  --

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

-- String comprehension --------------------------------------------------

lowers :: String -> Int
lowers str = length [x | x <- str, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [y | y <- xs, y == x]

char2int :: Char -> Int
char2int c = ord c - ord 'a'

int2char :: Int -> Char
int2char n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2char ((char2int c + n) `mod` 26)
          | otherwise = c 

encode :: Int -> String -> String
encode n str = [shift n x | x <- str]



-- Övningar --------------------------------------------------------------

pyths :: Int -> [(Int, Int, Int)]
pyths n = [ (a, b, c) | a <- [1..n], b <- [a..n], c <- [b..n], (a * a + b * b) == c * c, a + b + c <= n]


factors :: Int -> [Int]
factors n = [x | x <- [1..n], mod n x == 0]


isPerfect :: Int -> Bool
isPerfect n | sum (factors n) == n + n              = True
            | otherwise                             = False

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], isPerfect x]

isPrime :: Int -> Bool
isPrime n | factors n == [1,n]  = True
          | otherwise           = False

primesTo :: Int -> [Int]
primesTo n = [x | x <- [2..n], isPrime x]

sumTo :: [Int] -> Int -> [(Int, Int)]
sumTo xs n = [(x, y) | x <- xs, y <- xs, x <= y && x + y == n]


scalar :: [Int] -> [Int] -> Int
scalar [] []            = 0
scalar (x:xs) (y:ys)    = x * y + scalar xs ys

scalar2 :: [Int] -> [Int] -> Int
scalar2 xs ys = sum [x * y | (x,y) <- zip xs ys]

--------------------------- Kapitel 6: Recursion ---------------------------------------------------------------------------------------------------------------

-- Basics --
-- fac :: Int -> Int
-- fac n = product [1..n]

fac1 :: Int -> Int
fac1 0  = 1
fac1 n = n * fac1 (n-1)

-- Recursions on lists --








--------------------------- Kapitel 5 --------------------------------------------------------------------------------------------------------------------------
