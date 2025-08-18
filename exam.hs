{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}



interleave :: [a] -> [a] -> [a]
interleave [] []            = []
interleave [] (y:ys)        = y:ys
interleave (x:xs) []        = x:xs
interleave [a] [b]          = [a,b]
interleave (x:xs) (y:ys)    = interleave [x] [y] ++ interleave xs ys

prop_sum :: [Int] -> [Int] -> Bool
prop_sum xs ys = (sum xs + sum ys) == sum (interleave xs ys)

fun :: [Int] -> Int
fun []      = 10
fun [x]     = x
fun (x:xs)  = 1 + fun xs


data Role = Member| Moderator | Bot
    deriving (Show, Eq)


data User = User {
                    name        :: String,
                    role        :: Role
} deriving (Show, Eq)

data Post = Post {
                    message     :: String,
                    poster      :: User
 } deriving (Show, Eq)

data Channel = Channel {
                        topic           :: String,
                        posts           :: [Post]
} deriving (Show, Eq)

data Application = Application {
                                server      :: String,
                                users       :: [User],
                                channels    :: [Channel]
                    
}deriving (Show, Eq)

getRole :: String -> IO String
getRole _ = return "Admin"

readUser :: IO (String, String)
readUser = do
    putStrLn "Please give your username:"
    userName <- getLine
    userRole <- getRole userName
    return (userName, userRole)

data Some a = One a | Two a a deriving (Eq, Show)

toList :: Some a -> [a]
toList (One x)      = [x]
toList (Two x y)    = [x,y]

flatten :: [Some a] -> [a]
flatten []      = []
flatten (x:xs)  = toList x ++ flatten xs











----- 2023-10-25 ----------------------------------------------------------------------------------------------------------------

-- 1: -------------------------------------------------------------------------
-- a) f [4,7,1] = 7

-- b) f :: [Int] -> Int


-- 2: -------------------------------------------------------------------------

-- a) definera remove :: Int -> [Int] -> [Int]
remove :: Int -> [Int] -> [Int]
remove n xs = [x | x <- xs, x /= n]

-- b) använd remove och definera removeDups :: [Int] -> [Int]

removeDups :: [Int] -> [Int]
removeDups []       = []
removeDups (x:xs)   =  x : removeDups (remove x xs)

-- 3: -------------------------------------------------------------------------
data Position = Defender | Midfielder | Striker deriving (Eq, Show)

data Card = Yellow | Red deriving (Eq, Show)

data Player = Player {
                    playerName        :: String,
                    position    :: Position,
                    cards       :: [Card]
} deriving (Eq, Show)

data Club = Club {
                clubName        :: String,
                players     :: [Player]
} deriving (Eq, Show)

-- 4: -------------------------------------------------------------------------

-- Hjälpfunktion för en spelare --
choosePlayer :: IO String
choosePlayer = do
    putStrLn "What is the players name?"
    name <- getLine
    return name
{-
teamSelection :: IO [String]
teamSelection = do
    putStrLn "How many players?"
    n <- readLn
    return [replicateM n choosePlayer]
--}

-- 5: -------------------------------------------------------------------------

data Tree = Leaf Int | Node Tree Int Tree deriving (Eq, Show)

t :: Tree
t = Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Leaf 5)

-- a) definera root :: Tree -> Int
root :: Tree -> Int
root (Leaf a) = a
root (Node left a right) = a

-- b) definera mirror :: Tree -> Tree
mirror :: Tree -> Tree
mirror (Leaf a) = (Leaf a)
mirror (Node left a right) = (Node (mirror right) a (mirror left))

-- 6: -------------------------------------------------------------------------

-- a) prop_mirror 
prop_mirror :: Tree -> Bool
prop_mirror tree = tree == mirror (mirror tree)

-- b) prop_root
prop_root :: Tree -> Bool
prop_root tree = root tree == root (mirror tree)
