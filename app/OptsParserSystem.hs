{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-hugo.poggetti
-- File description:
-- Opt
-}

module OptsParserSystem (Opts(Opts),outputFile, outputFormat, inputFile, inputFormat, optsParser, optsToList, globalOptsChecker, usage) where

data Opts = Opts {
    inputFile :: Maybe String,
    inputFormat :: Maybe String,
    outputFile :: Maybe String,
    outputFormat :: Maybe String
} deriving (Eq, Show)

getNext :: Maybe Int -> [String] -> Maybe String
getNext Nothing _ = Nothing
getNext (Just i) args
    | (i + 1) == length args = Nothing
    | otherwise = Just (args !! (i + 1))

findOption :: String -> [String] -> Int -> Maybe Int
findOption _ [] _ = Nothing
findOption opt (x:xs) i
    | opt == x = Just i
    | otherwise = findOption opt xs (i + 1)

optsParser :: [String] -> Opts
optsParser [] = Opts {
        inputFile = Nothing,
        inputFormat = Nothing,
        outputFile = Nothing,
        outputFormat = Nothing
    }
optsParser args = Opts {
        inputFile = getNext (findOption "-i" args 0) args,
        inputFormat = getNext (findOption "-e" args 0) args,
        outputFile = getNext (findOption "-o" args 0) args,
        outputFormat = getNext (findOption "-f" args 0) args
    }

optsChecker :: Opts -> Bool
optsChecker (Opts {inputFile = Nothing, inputFormat = _,
    outputFile = _, outputFormat = _}) = False
optsChecker (Opts {inputFile = _, inputFormat = _,
    outputFile = _, outputFormat = Nothing}) = False
optsChecker (Opts {inputFile = _, inputFormat = _,
    outputFile = _, outputFormat = _}) = True

removeElem :: [String] -> String -> [String]
removeElem _ [] = []
removeElem [] _ = []
removeElem (x:xs) str
    | str == x = removeElem xs str
    | otherwise = x : removeElem xs str

getRest :: [String] -> [String] -> [String]
getRest [] _ = []
getRest args [] = args
getRest args (x:xs) = getRest (removeElem args x) xs

optsToList :: Opts -> [String]
optsToList (Opts {inputFile = Nothing, inputFormat = _,
    outputFile = _, outputFormat = _}) = []
optsToList (Opts {inputFile = _, inputFormat = _,
    outputFile = _, outputFormat = Nothing}) = []
optsToList (Opts {inputFile = (Just i), inputFormat = Nothing,
    outputFile = Nothing, outputFormat = (Just f)}) =
        ["-i", i, "-f", f]
optsToList (Opts {inputFile = (Just i), inputFormat = (Just e),
    outputFile = Nothing, outputFormat = (Just f)}) =
        ["-i", i, "-e", e, "-f", f]
optsToList (Opts {inputFile = (Just i), inputFormat = Nothing,
    outputFile = (Just o), outputFormat = (Just f)}) =
        ["-i", i, "-o", o, "-f", f]
optsToList (Opts {inputFile = (Just i), inputFormat = (Just e),
    outputFile = (Just o), outputFormat = (Just f)}) =
        ["-i", i, "-e", e, "-o", o, "-f", f]

globalOptsChecker :: [String] -> Opts -> Bool
globalOptsChecker args opts
    | optsChecker opts == False ||
        length (getRest args (optsToList opts)) /= 0 = False
    | otherwise = True

usage :: String
usage = "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]\n\n" ++
        "    ifile\tpath to the file to convert\n" ++
        "    oformat\toutput format (xml, json, markdown)\n" ++
        "    ofile\tpath to the output file\n" ++
        "    iformat\tinput format (xml, json, markdown)"
