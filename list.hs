import System.IO
import Data.List
import Data.Maybe (listToMaybe)

data Movies = Movie
    { name   :: String
    , link   :: String
    , actors :: [String]
    , genre  :: String
    , year   :: Int
    , rating :: Int
    } deriving (Show, Read)

-- Ask for the movie's details and store them in the file
input :: IO ()
input = do
    -- Prompt and get the movie name
    putStrLn "  Enter title:"
    movieName <- getLine

    -- Prompt and get the link
    putStrLn "  Enter link:"
    movieLink <- getLine

    -- Prompt and get the actors (comma separated)
    putStrLn "  Starring:"
    actorInput <- getLine
    let actorsList = words (map (\c -> if c == ',' then ' ' else c) actorInput) -- Convert commas to spaces to split

    -- Prompt and get the genre
    putStrLn "  Category"
    movieGenre <- getLine

    -- Prompt and get the year
    putStrLn "  Release year:"
    movieYear <- readLn :: IO Int

    -- Prompt and get the rating
    putStrLn "  Rating:"
    movieRating <- readLn :: IO Int

    -- Create the movie entry
    let newMovie = Movie
            { name = movieName
            , link = movieLink
            , actors = actorsList
            , genre = movieGenre
            , year = movieYear
            , rating = movieRating
            }

    -- Write the movie entry to a file (output.txt)
    appendFile "output.txt" (show newMovie ++ "\n")

    -- Notify user
    putStrLn "  User input saved."
    putStrLn "    Continue? y/n"
    reply <- getLine
    if reply == "y"
        then input
        else putStrLn "  Exiting.."

-- Function to show all movie entries in a user-friendly format
outputAll :: IO ()
outputAll = do
    -- Read the contents of the file
    content <- readFile "output.txt"

    -- Split the content into individual movie entries
    let movies = map read (lines content) :: [Movies]  -- Convert each line into a Movie

    -- Show all movies in a formatted way
    putStrLn "------ All entries ------"
    mapM_ formatMovie movies

-- Function to display movie details nicely
-- Function to display movie details nicely (with a simpler character)
formatMovie :: Movies -> IO ()
formatMovie movie = do
    putStrLn $ "\n -- Title: " ++ name movie
    putStrLn $ " -- Category: " ++ genre movie
    putStrLn $ " -- Year: " ++ show (year movie)
    putStrLn $ " -- Rating: " ++ show (rating movie) ++ "/10"
    putStrLn $ " -- Starring: " ++ unwords (map (\actor -> actor ++ ", ") (actors movie))
    putStrLn $ " -- Link: " ++ link movie


-- Function to retrieve and filter movies by genre
category :: String -> IO ()
category genreToFind = do
    -- Read the file
    content <- readFile "output.txt"

    -- Split the content into individual movie entries
    let movies = map read (lines content) :: [Movies]  -- Convert each line into a Movie
    
    -- Filter movies by the genre
    let filteredMovies = filter (\movie -> genre movie == genreToFind) movies

    -- Show the filtered movies in a formatted way
    putStrLn $ "------ Category: " ++ genreToFind ++ " ------"
    mapM_ formatMovie filteredMovies

-- Function to retrieve and filter movies by actor
starring :: String -> IO ()
starring actorToFind = do
    -- Read the file
    content <- readFile "output.txt"

    -- Split the content into individual movie entries
    let movies = map read (lines content) :: [Movies]  -- Convert each line into a Movie
    
    -- Filter movies by the actor
    let filteredMovies = filter (\movie -> actorToFind `elem` actors movie) movies

    -- Show the filtered movies in a formatted way
    putStrLn $ "------ Entries starring: " ++ actorToFind ++ " ------"
    mapM_ formatMovie filteredMovies

-- Function to wipe the output.txt file
wipe :: IO()
wipe = do
    putStrLn $ "  Are you sure? (y/n)"
    wipeReply <- getLine
    if wipeReply == "y"
        then do
            putStrLn "  Really?"
            really <- getLine
            if really == "Really."
                then do
                    writeFile "output.txt" "" 
                    putStrLn "  Data wiped."
            else putStrLn "  Wipe aborted."
        else putStrLn "  Aborting wipe.."

-- Main entry point to allow the user to input, view, or wipe data
main :: IO ()
main = do
    putStrLn "------ Choose an option ------"
    putStrLn "  1. Input a new entry"
    putStrLn "  2. Show all entries"
    putStrLn "  3. Search: Category"
    putStrLn "  4. Search: Starring"
    
    option <- getLine
    case option of
        "1" -> input
        "2" -> outputAll
        "3" -> do
            putStrLn "  Enter category:"
            categoryInput <- getLine
            category categoryInput
        "4" -> do
            putStrLn "  Enter actor:"
            actorInput <- getLine
            starring actorInput
        _   -> putStrLn "  Invalid option, please try again."
