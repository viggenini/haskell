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
n = a `div` (length xs)
    where
        a = 10
        xs = [1,2,3,4,5]

-- 4: Definera last ------------------------

last1 xs = drop ( (length xs) - 1) xs

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

ztodd :: Int -> [Int]
ztodd n = [x|x <- [0..n], not (even x)]

