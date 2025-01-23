import Data.List
import Data.Char (chr, ord, isAlpha)

prod :: [Int] -> Int
prod xs = product xs

-- indexera alla element i lista med udda index
odds :: [a] -> [a]
odds xs = [x | (i, x) <- zip [0..] xs, odd i]

takes :: Int -> [a] -> [a]
takes n _ | n <= 0 =  []
takes _ []         =  []
takes n (x:xs)     =  x : takes (n-1) xs

splitIndex :: [a] -> [a]
splitIndex xs = [x | (i, x) <- zip [0..] xs] 

splitDeez :: Int -> [a] -> ([a], [a])
splitDeez _ [] = ([], [])
splitDeez n xs  | n <= 0 =  ([], xs)
                |otherwise = let (left, right) = splitDeez (n-1) (tail xs)
                    in (head xs : left, right)

isPermutation :: (Eq a, Ord a) => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation xs ys | sort xs == sort ys = True
                    |otherwise = False

-- 3.4 -- forsätt på resten i vecka 3
unzip1 :: [(a, b)] -> ([a], [b])
unzip1 [] = ([], [])
unzip1 xs = (map fst xs, map snd xs)

-- Week 4 -----------------------------------------------------------------

-- 4.1 --
printFullName :: IO ()
printFullName = do
    putStrLn "What is your first name?"
    firstName <- getLine
    
    putStrLn  "What is your last name?"
    lastName <- getLine

    putStrLn ("Hello " ++ firstName ++ " " ++ lastName)

-- 4.2 --
isConsonant :: Char -> Bool
isConsonant x 
    | x `elem` "aeiouöåä"   = False
    |otherwise              = True

toRobber :: String -> String
toRobber str = concatMap (\c -> if isConsonant c then [c] ++ "o" ++ [c] ++ " " else [c] ++ " ") str

robber :: IO ()
robber = do  
    putStrLn "Enter message to encrypt:"
    message <- getLine
    putStrLn(toRobber message)

pascal :: Int -> [Int]
pascal 1 = [1]
pascal n = undefined


--------------------------------------------------- Eget -----------------------------------------------------------
toCaesar :: String -> String
toCaesar msg = map shiftRight msg
    where
        shiftRight c
            |isAlpha c = if c `elem` ['a'..'z']
                    then chr $ (ord c - ord 'a' + 3) `mod` 26 + ord 'a'
                    else chr $ (ord c - ord 'A' + 3) `mod` 26 + ord 'A'
            |otherwise = c

fromCaesar :: String -> String
fromCaesar msg = map shiftLeft msg
  where
    shiftLeft c
      | isAlpha c = 
          if c `elem` ['a'..'z']
            then chr $ ((ord c - ord 'a' - 3 + 26) `mod` 26) + ord 'a'
            else chr $ ((ord c - ord 'A' - 3 + 26) `mod` 26) + ord 'A'
      | otherwise = c

caesar :: IO ()
caesar = do 
        putStrLn "Enter message to encrypt:"
        message <- getLine
        putStrLn("Encrypting.." ++ "\n" ++ "Your message is: " ++ "\n" ++ toCaesar message)
        putStrLn("Translate back?" ++ "\n" ++ "y/n")
        reply <- getLine
        if reply == "y" then putStrLn("Decrypting.." ++ "\n" ++ fromCaesar (toCaesar message)) else putStrLn("Ending sequence..")
