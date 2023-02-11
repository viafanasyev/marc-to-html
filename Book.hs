{-# LANGUAGE OverloadedStrings #-}

module Book
    ( Book (..)
    , Author
    , Title
    , Html
    , bookToHtml
    , booksToHtml
    ) where

import qualified Data.Text as T

type Author = T.Text
type Title = T.Text

data Book = Book
    { author :: Author
    , title  :: Title
    } deriving Show

type Html = T.Text

bookToHtml :: Book -> Html
bookToHtml book = mconcat
    [ "<p>\n"
    , titleInTags
    , authorInTags
    , "</p>\n"
    ]
  where
    titleInTags = mconcat [ "<strong>", title book, "</strong>\n" ]
    authorInTags = mconcat [ "<em>", author book, "</em>\n" ]

booksToHtml :: [Book] -> Html
booksToHtml books = mconcat
    [ "<html>\n"
    , "<head><title>Books</title>"
    , "<meta charset='utf-8'/>"
    , "</head>\n"
    , "<body>\n"
    , booksHtml
    , "</body>\n"
    , "</html>"
    ]
  where
    booksHtml = mconcat $ map bookToHtml books
