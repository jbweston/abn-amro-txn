module Main where

import qualified Data.ByteString as BS
import Text.Megaparsec
import Data.ABNAmro.MT940 hiding (parse)

import System.Environment

main :: IO ()
main = do
    fileName <- head <$> getArgs
    result <- parse messages fileName <$> BS.readFile fileName
    case result of
        Left a -> putStrLn $ errorBundlePretty a
        Right a -> print a
    where
        messages = some parser :: Parser [MT940Message]
