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

main :: IO ()
main = do
    putStrLn "lib test\n"

