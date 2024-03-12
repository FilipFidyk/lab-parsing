module Main (main) where

import Text.Megaparsec
import Parsing

main :: IO ()
main = do
    line <- getLine
    case parse parseExpr "" line of
        (Left err) -> putStrLn $ errorBundlePretty err
        (Right x) -> putStrLn $ show $ eval x