{-# LANGUAGE OverloadedStrings #-}

module MarcReader
    ( allRecords
    , readRecords
    ) where

import           Book               (Book (..))
import qualified Book
import qualified Data.ByteString    as B
import           Data.Maybe         (fromJust, isJust)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as E

newtype MarcRecordRaw = MarcRecordRaw B.ByteString
newtype MarcLeaderRaw = MarcLeaderRaw B.ByteString
newtype MarcDirectoryRaw = MarcDirectoryRaw B.ByteString
newtype MarcDirectoryEntryRaw = MarcDirectoryEntryRaw B.ByteString
newtype MarcBaseRaw = MarcBaseRaw B.ByteString

leaderLength :: Int
leaderLength = 24

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader (MarcRecordRaw record) = MarcLeaderRaw $ B.take leaderLength record

rawToInt :: B.ByteString -> Int
rawToInt = read . T.unpack . E.decodeUtf8

recordLengthLength :: Int
recordLengthLength = 5

getRawRecordLength :: B.ByteString -> Int
getRawRecordLength = rawToInt . B.take recordLengthLength

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength (MarcLeaderRaw leader) = getRawRecordLength leader

nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest marcStream = (MarcRecordRaw record, rest)
  where
    recordLength = getRawRecordLength marcStream
    (record, rest) = B.splitAt recordLength marcStream

allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords marcStream
    | B.null marcStream = []
    | otherwise = next : allRecords rest
  where
    (next, rest) = nextAndRest marcStream

baseAddressOffset :: Int
baseAddressOffset = 12

baseAddressLength :: Int
baseAddressLength = 5

getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress (MarcLeaderRaw leader) = rawToInt $ B.take baseAddressLength remainder
  where
    remainder = B.drop baseAddressOffset leader

getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - leaderLength

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record@(MarcRecordRaw recordText) = MarcDirectoryRaw $ B.take directoryLength afterLeader
  where
    directoryLength = getDirectoryLength $ getLeader record
    afterLeader = B.drop leaderLength recordText

dirEntryLength :: Int
dirEntryLength = 12

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory (MarcDirectoryRaw directory) = splitHelper directory
  where
    splitHelper :: B.ByteString -> [MarcDirectoryEntryRaw]
    splitHelper directoryText
        | B.null directoryText = []
        | otherwise = MarcDirectoryEntryRaw nextEntry : splitHelper restEntries
      where
        (nextEntry, restEntries) = B.splitAt dirEntryLength directoryText

data FieldMetadata = FieldMetadata
    { fieldTag    :: T.Text
    , fieldLength :: Int
    , fieldStart  :: Int
    } deriving Show

fieldTagLength :: Int
fieldTagLength = 3

fieldLengthLength :: Int
fieldLengthLength = 4

makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata (MarcDirectoryEntryRaw entry) = FieldMetadata tag length start
  where
    (rawTag, rest) = B.splitAt fieldTagLength entry
    tag = E.decodeUtf8 rawTag
    (rawLength, rawStart) = B.splitAt fieldLengthLength rest
    length = rawToInt rawLength
    start = rawToInt rawStart

getFieldMetadata :: [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata = map makeFieldMetadata

getBase :: MarcRecordRaw -> MarcBaseRaw
getBase record@(MarcRecordRaw recordText) = MarcBaseRaw $ B.drop baseAddress recordText
  where
    leader = getLeader record
    baseAddress = getBaseAddress leader

type FieldText = T.Text

getTextField :: MarcRecordRaw -> FieldMetadata -> FieldText
getTextField record fieldMetadata = E.decodeUtf8 byteStringValue
  where
    (MarcBaseRaw baseText) = getBase record
    baseAtEntry = B.drop (fieldStart fieldMetadata) baseText
    byteStringValue = B.take (fieldLength fieldMetadata) baseAtEntry

fieldDelimeter :: Char
fieldDelimeter = toEnum 31

titleTag :: T.Text
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

authorTag :: T.Text
authorTag = "100"

authorSubfield :: Char
authorSubfield = 'a'

lookupFieldMetadata :: T.Text -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata tag record
    | length results < 1 = Nothing
    | otherwise = Just $ head results
  where
    metadata = getFieldMetadata $ splitDirectory $ getDirectory $ record
    results = filter ((== tag) . fieldTag) metadata

lookupSubfield :: Maybe FieldMetadata -> Char -> MarcRecordRaw -> Maybe T.Text
lookupSubfield Nothing _ _ = Nothing
lookupSubfield (Just fieldMetadata) subfield record
    | null results = Nothing
    | otherwise = Just $ T.drop 1 $ head results
  where
    rawField = getTextField record fieldMetadata
    subfields = T.split (== fieldDelimeter) rawField
    results = filter ((== subfield) . T.head) subfields

lookupValue :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text
lookupValue tag subfield record = lookupSubfield entryMetadata subfield record
  where
    entryMetadata = lookupFieldMetadata tag record

lookupTitle :: MarcRecordRaw -> Maybe Book.Title
lookupTitle = lookupValue titleTag titleSubfield

lookupAuthor :: MarcRecordRaw -> Maybe Book.Author
lookupAuthor = lookupValue authorTag authorSubfield

marcToPairs :: B.ByteString -> [(Maybe Book.Title, Maybe Book.Author)]
marcToPairs marcStream = zip titles authors
  where
    records = allRecords marcStream
    titles = map lookupTitle records
    authors = map lookupAuthor records

pairsToBooks :: [(Maybe Book.Title, Maybe Book.Author)] -> [Book]
pairsToBooks pairs = map (\(title, author) -> Book
    { title = fromJust title
    , author = fromJust author
    }) justPairs
  where
    justPairs = filter (\(title, author) -> isJust title && isJust author) pairs

readRecords :: B.ByteString -> [Book]
readRecords = pairsToBooks . marcToPairs
