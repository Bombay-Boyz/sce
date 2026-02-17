{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module SCE.Core.DataModel
  ( -- * CSV Parsing
    parseCSV
  , parseCSVFile
    -- * Table Construction
  , buildDataTable
  , inferColumnScales
    -- * Data Access
  , getColumn
  , getNumericColumn
  , getRow
  ) where

import SCE.Core.Types
import SCE.Core.MeasurementScale

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL

import qualified Data.Csv as Csv
import Data.Csv (decodeByName)

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import qualified Data.HashMap.Strict as HM

import Text.Read (readMaybe)
import Control.Exception (try, IOException, SomeException)
import System.FilePath (normalise, takeDirectory)
import System.Directory (doesFileExist, canonicalizePath)
import Data.List (isPrefixOf)

------------------------------------------------------------
-- Path Validation
------------------------------------------------------------

-- | Validate and normalize file path for security
-- Uses canonicalization to prevent path traversal attacks
-- Optionally restricts access to a specific allowed directory
validateFilePath :: Maybe FilePath -> FilePath -> IO (Either DetailedError FilePath)
validateFilePath maybeAllowedDir path = do
  if null path
    then return $ Left (mkError E1001 "Empty file path provided"
                          ["Provide a non-empty file path."] Error)
    else do
      canonResult <- try (canonicalizePath path) :: IO (Either SomeException FilePath)
      case canonResult of
        Left _ ->
          return $ Left (mkError E1001 ("File not found: " <> T.pack path)
                           ["Check that the file exists and the path is correct."] Error)
        Right canonPath -> do
          exists <- doesFileExist canonPath
          if not exists
            then return $ Left (mkError E1001 ("File does not exist: " <> T.pack canonPath)
                                  ["Check that the file path is correct."] Error)
            else case maybeAllowedDir of
              Nothing ->
                return $ Right canonPath
              Just allowedDir -> do
                allowedCanon <- canonicalizePath allowedDir
                if allowedCanon `isPrefixOf` canonPath
                  then return $ Right canonPath
                  else return $ Left (mkError E1001
                                       ("File is outside the allowed directory: " <> T.pack canonPath)
                                       ["Move the file to an allowed location."] Error)

------------------------------------------------------------
-- CSV Parsing
------------------------------------------------------------

parseCSV :: ByteString -> SCEResult (Vector ColumnName, Vector DataRow)
parseCSV bs =
  case decodeByName bs of
    Left err -> Left (mkError E1001 (T.pack err) ["Check the CSV file format and encoding."] Error)
    Right (header, rows) -> do
      let columns = V.map TE.decodeUtf8 header
      dataRows <- V.mapM (rowToDataRow columns) rows
      return (columns, dataRows)

parseCSVFile
  :: FilePath
  -> IO (SCEResult (Vector ColumnName, Vector DataRow))
parseCSVFile path = do
  -- Validate path (no directory restriction for backward compatibility)
  -- In production, you should pass an allowed directory
  pathResult <- validateFilePath Nothing path
  case pathResult of
    Left err -> return $ Left err
    Right validPath -> do
      result <- try (BL.readFile validPath) :: IO (Either IOException ByteString)
      case result of
        Left ioErr -> 
          return $ Left (mkError E1001 ("Failed to read file '" <> T.pack validPath <> "': " <> T.pack (show ioErr)) ["Check that the file exists and is readable."] Error)
        Right contents -> 
          return $ parseCSV contents

------------------------------------------------------------
-- Row Conversion
------------------------------------------------------------

rowToDataRow
  :: Vector ColumnName
  -> Csv.NamedRecord
  -> SCEResult DataRow
rowToDataRow columns record = do
  entries <- V.mapM (parseField record) columns
  return $ M.fromList $ V.toList $ V.zip columns entries
  where
    parseField
      :: Csv.NamedRecord
      -> ColumnName
      -> SCEResult DataValue
    parseField rec colName =
      let key = TE.encodeUtf8 colName
      in case HM.lookup key rec of
           Nothing  -> Right MissingValue
           Just val -> Right $ parseValue $ TE.decodeUtf8 val

    parseValue :: Text -> DataValue
    parseValue txt
      | T.null txt = MissingValue
      | Just i <- readMaybe (T.unpack txt) :: Maybe Int
          = IntegerValue i
      | Just d <- readMaybe (T.unpack txt) :: Maybe Double
          = NumericValue d
      | otherwise = TextValue txt

------------------------------------------------------------
-- Table Construction
------------------------------------------------------------

buildDataTable
  :: Maybe Text
  -> Maybe FilePath
  -> Vector ColumnName
  -> Vector DataRow
  -> SCEResult DataTable
buildDataTable title source columns rows
  | V.null columns = Left (mkError E1004 "No columns found in CSV data" ["Ensure the CSV file has a header row and at least one column."] Error)
  | V.null rows    = Left (mkError E1004 "No data rows found in CSV file" ["Ensure the CSV file contains at least one data row after the header."] Error)
  | otherwise      =
      let scales = inferColumnScales columns rows
          metadata = TableMetadata
            { metaTitle       = title
            , metaSource      = source
            , metaDescription = Nothing
            , metaRowCount    = V.length rows
            , metaColCount    = V.length columns
            }
      in Right DataTable
          { tableMetadata = metadata
          , tableColumns  = columns
          , tableRows     = rows
          , tableScales   = scales
          }

------------------------------------------------------------
-- Scale Inference
------------------------------------------------------------

inferColumnScales
  :: Vector ColumnName
  -> Vector DataRow
  -> Map ColumnName MeasurementScale
inferColumnScales columns rows =
  M.fromList $ V.toList $ V.map inferColumnScale columns
  where
    inferColumnScale :: ColumnName -> (ColumnName, MeasurementScale)
    inferColumnScale colName =
      let values = V.mapMaybe (M.lookup colName) rows
          scale  = inferScale $ V.toList values
      in (colName, scale)

------------------------------------------------------------
-- Data Access
------------------------------------------------------------

getColumn
  :: ColumnName
  -> DataTable
  -> SCEResult (Vector DataValue)
getColumn colName table
  | colName `V.notElem` tableColumns table =
      Left (mkError E2004 ("Column not found: " <> colName) ["Check the column name spelling; column names are case-sensitive."] Error)
  | otherwise =
      Right $ V.mapMaybe (M.lookup colName) (tableRows table)

getNumericColumn
  :: ColumnName
  -> DataTable
  -> SCEResult (Vector Double)
getNumericColumn colName table = do
  col <- getColumn colName table
  V.mapM toNumeric col
  where
    toNumeric :: DataValue -> SCEResult Double
    toNumeric (NumericValue d) = Right d
    toNumeric (IntegerValue i) = Right $ fromIntegral i
    toNumeric (TextValue t) =
      case readMaybe (T.unpack t) of
        Just d  -> Right d
        Nothing -> Left (mkError E2001 ("Cannot convert value to numeric: " <> t) ["Ensure the column contains only numeric data."] Error)
    toNumeric MissingValue =
      Left (mkError E2004 "Missing value encountered in numeric column" ["Filter missing values before numeric operations, or use completeCaseFilter."] Error)

getRow
  :: Int
  -> DataTable
  -> SCEResult DataRow
getRow idx table
  | idx < 0 || idx >= V.length (tableRows table) =
      Left (mkError E2001 "Row index out of bounds" ["Provide a row index within the valid range [0, rowCount-1]."] Error)
  | otherwise =
      Right $ tableRows table V.! idx
