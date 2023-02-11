{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Book            (Html)
import qualified Book
import qualified Data.ByteString as B
import qualified Data.Text.IO    as TIO
import qualified MarcReader      as MRC

processRecords :: Int -> B.ByteString -> Html
processRecords n = Book.booksToHtml . take n . MRC.readRecords

main :: IO ()
main = do
    marcData <- B.readFile "sample.mrc"
    let processed = processRecords 500 marcData
    TIO.writeFile "books.html" processed
