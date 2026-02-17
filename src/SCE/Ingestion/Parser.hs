{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData         #-}

module SCE.Ingestion.Parser
  ( parseCSVBytes
  , parseCSVFile
  , detectDelimiter
  , RawFrame(..)
  , RawRow
  , ParseConfig(..)
  , defaultParseConfig
  ) where

import SCE.Core.Types
  ( DetailedError
  , ErrorCode(..)
  , Severity(..)
  , mkError
  )

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.ByteString.Lazy        (ByteString)
import Data.Int (Int64)
import qualified Data.Csv                    as Csv
import           Data.Csv                    (decodeWith, HasHeader(..), DecodeOptions(..))

import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V

import qualified Data.Text                   as T
import           Data.Text                   (Text)
import qualified Data.Text.Encoding          as TE

import qualified Data.HashMap.Strict         as HM   -- ✅ ADDED

import           Data.List                   (maximumBy)
import           Data.Ord                    (comparing)
import           Data.Char                   (ord)

import           Control.Exception           (try, IOException, SomeException)
import           System.Directory            (doesFileExist, canonicalizePath)

------------------------------------------------------------
-- Types
------------------------------------------------------------

type RawRow = Vector Text

data ParseConfig = ParseConfig
  { pcDelimiter   :: Maybe Char
  , pcHasHeader   :: Bool
  , pcQuoteChar   :: Char
  , pcCommentChar :: Maybe Char
  , pcMaxRows     :: Maybe Int
  } deriving stock (Show, Eq)

defaultParseConfig :: ParseConfig
defaultParseConfig = ParseConfig
  { pcDelimiter   = Nothing
  , pcHasHeader   = True
  , pcQuoteChar   = '"'
  , pcCommentChar = Nothing
  , pcMaxRows     = Nothing
  }

data RawFrame = RawFrame
  { rfColumns  :: Vector Text
  , rfRows     :: Vector RawRow
  , rfSource   :: Maybe FilePath
  , rfRowCount :: Int
  , rfColCount :: Int
  } deriving stock (Show)

------------------------------------------------------------
-- Delimiter detection
------------------------------------------------------------

detectDelimiter :: ByteString -> Either DetailedError Char
detectDelimiter bs =
  let candidates = [',', '\t', ';', '|']
      sampleLines = take 5 $ filter (not . BLC.null) $ BLC.lines bs
  in if null sampleLines
       then Left (mkError E1001 "Cannot detect delimiter: file appears to be empty"
                    ["Provide a non-empty CSV file."] Error)
       else
         let scores = [(c, scoreDelimiter c sampleLines) | c <- candidates]
             (best, bestScore) = maximumBy (comparing snd) scores
         in if bestScore == 0
            then Right ','   -- FIX: treat as single-column CSV
            else Right best
            
  where
    scoreDelimiter :: Char -> [ByteString] -> Int64
    scoreDelimiter c ls =
                     let counts = map (BLC.count c) ls
                         in minimum counts



------------------------------------------------------------
-- Core parsing
------------------------------------------------------------

parseCSVBytes :: ParseConfig -> ByteString -> Either DetailedError RawFrame
parseCSVBytes cfg bs = do
  let filtered = applyCommentFilter (pcCommentChar cfg) bs

  delim <- case pcDelimiter cfg of
    Just c  -> Right c
    Nothing -> detectDelimiter filtered   -- FIX: detect after removing comments

  let opts = Csv.defaultDecodeOptions
               { Csv.decDelimiter = fromIntegral (ord delim) }

  if pcHasHeader cfg
    then parseWithHeader cfg opts filtered
    else parseWithoutHeader cfg opts delim filtered

parseCSVFile :: ParseConfig -> FilePath -> IO (Either DetailedError RawFrame)
parseCSVFile cfg path = do
  pathResult <- validateFilePath path
  case pathResult of
    Left err -> return $ Left err
    Right validPath -> do
      readResult <- try (BL.readFile validPath) :: IO (Either IOException ByteString)
      case readResult of
        Left ioErr ->
          return $ Left $ mkError E1001
            ("Failed to read file '" <> T.pack validPath <> "': " <> T.pack (show ioErr))
            ["Check file permissions."] Error
        Right contents ->
          return $ fmap (\rf -> rf { rfSource = Just validPath })
                        (parseCSVBytes cfg contents)

------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------

parseWithHeader
  :: ParseConfig
  -> DecodeOptions
  -> ByteString
  -> Either DetailedError RawFrame
parseWithHeader cfg opts bs =
  case Csv.decodeByNameWith opts bs of
    Left err ->
      Left $ mkError E1001 (T.pack err) ["Check the CSV file format and header row."] Error
    Right (header, rows) ->
      let cols    = V.map TE.decodeUtf8 header
          rawRows = V.map (headerRowToRaw cols) rows
          limited = applyMaxRows (pcMaxRows cfg) rawRows
      in if V.null cols
           then Left $ mkError E1004 "CSV header is empty" ["Ensure the file has a header row."] Error
           else Right RawFrame
                  { rfColumns  = cols
                  , rfRows     = limited
                  , rfSource   = Nothing
                  , rfRowCount = V.length limited
                  , rfColCount = V.length cols
                  }
  where
    headerRowToRaw :: Vector Text -> Csv.NamedRecord -> RawRow
    headerRowToRaw cols rec =
      V.map
        (\col ->
           maybe ""
                 TE.decodeUtf8
                 (HM.lookup (TE.encodeUtf8 col) rec)
        )
        cols

parseWithoutHeader
  :: ParseConfig
  -> DecodeOptions
  -> Char
  -> ByteString
  -> Either DetailedError RawFrame
parseWithoutHeader cfg opts _delim bs =
  case decodeWith opts NoHeader bs :: Either String (Vector (Vector BL.ByteString)) of
    Left err ->
      Left $ mkError E1001 (T.pack err) ["Check the CSV file format."] Error
    Right rows ->
      let rawRows  = V.map (V.map (TE.decodeUtf8 . BL.toStrict)) rows
          limited  = applyMaxRows (pcMaxRows cfg) rawRows
      in case V.uncons limited of
           Nothing ->
             Left $ mkError E1004 "CSV file contains no data rows" ["Ensure the file is non-empty."] Error
           Just (firstRow, _) ->
             let nCols    = V.length firstRow
                 colNames = V.fromList [T.pack ("col_" ++ show i) | i <- [1..nCols]]
             in Right RawFrame
                  { rfColumns  = colNames
                  , rfRows     = limited
                  , rfSource   = Nothing
                  , rfRowCount = V.length limited
                  , rfColCount = nCols
                  }

applyCommentFilter :: Maybe Char -> ByteString -> ByteString
applyCommentFilter Nothing  bs = bs
applyCommentFilter (Just c) bs =
  BLC.unlines
    $ filter (not . BLC.isPrefixOf (BLC.singleton c))
    $ BLC.lines bs

applyMaxRows :: Maybe Int -> Vector a -> Vector a
applyMaxRows Nothing    v = v
applyMaxRows (Just n)   v = V.take n v

validateFilePath :: FilePath -> IO (Either DetailedError FilePath)
validateFilePath path
  | null path =
      return $ Left $ mkError E1001 "Empty file path provided"
                        ["Provide a non-empty file path."] Error
  | otherwise = do
      canonResult <- try (canonicalizePath path) :: IO (Either SomeException FilePath)
      case canonResult of
        Left _ ->
          return $ Left $ mkError E1001 ("File not found: " <> T.pack path)
                            ["Check that the file exists and the path is correct."] Error
        Right canonPath -> do
          exists <- doesFileExist canonPath
          if not exists
            then return $ Left $ mkError E1001 ("File does not exist: " <> T.pack canonPath)
                                    ["Check that the file path is correct."] Error
            else return $ Right canonPath
