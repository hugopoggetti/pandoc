import Test.QuickCheck
import Lib (Parser, runParser,
        parseChar,
        parseUInt,
        parseInt,
        parseTuple,
        parseAnyChar,
        parseOr,
        parseAnd,
        parseAndWith,
        parseMany,
        parseSome)

runTests :: IO ()
runTests = do
    putStrLn "lib test\n"
    -- quickCheck runParser
