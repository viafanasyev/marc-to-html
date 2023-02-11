{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import qualified Data.Text.IO    as TIO
import qualified MarcToHtml      as M2H

main :: IO ()
main = do
    marcData <- B.readFile "sample.mrc"
    let processed = M2H.processRecords 500 marcData
    TIO.writeFile "books.html" processed
