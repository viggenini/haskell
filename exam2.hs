----------------------------- 2025 01 07 ------------------------------

-- 1: ---------------------------------------------------------------------------------------------

-- a)
-- 42

-- b)
-- fun :: [Int] -> Int

-- 2: ---------------------------------------------------------------------------------------------

chop :: Int -> [a] -> [[a]]
chop 0 [a]      = [[a]]
chop _ []       = [[]]
chop n xs       | n >= length xs    = [xs]
                | otherwise         = [fst (splitAt n (xs))] ++ chop n (snd (splitAt n (xs)))

-- 3: ---------------------------------------------------------------------------------------------

data Speciality = Programming | Logic_and_types | Security | Formal_methods deriving (Eq, Show)

data Card = Card {
                legendName      :: String,
                speciality      :: Speciality,
                year            :: Int
} deriving (Eq, Show)

data Player = Player {
                playerName      :: String,
                hand            :: [Card],
                fupsters        :: Int
} deriving (Eq, Show)

data Fupster = Fupster {
                players         :: [Player],
                deck            :: [Card]
} deriving (Eq, Show)

-- 4: ---------------------------------------------------------------------------------------------
{-
readCard :: IO (String, Int, String)
readCard = do
    putStrLn "Please give the legends name:"
    lName <- getLine
    putStrLn "Enter year of birth:"
    bYear <- readLn
    spec <- getSpeciality
    return (lName, bYear, spec)
-}
-- 5: ---------------------------------------------------------------------------------------------

data List = Empty | Skip List | Cons Int List deriving Show

as :: List
as = Cons 1 (Cons 2 (Skip (Cons 4 Empty)))

bs :: List
bs = Cons 1(Skip (Cons 3 (Skip (Cons 5 Empty))))

toList :: List -> [Maybe Int]
toList l = case l of
    Empty       -> []
    Skip l      -> Nothing : toList l
    Cons x l    -> Just x  : toList l


































