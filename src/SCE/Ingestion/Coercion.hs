{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData         #-}

{-|
Module      : SCE.Ingestion.Coercion
Description : Stage 3 of the ingestion pipeline — RawFrame to ValidatedFrame
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Applies a 'CommittedSchema' to a 'RawFrame', converting raw text cells
into typed vectors. Unparseable values become missing (with a 'CoercionWarning').
Returns 'Left' only when the resulting frame would have zero usable rows.
-}
module SCE.Ingestion.Coercion
  ( -- * Frame coercion
    coerceFrame
    -- * Types
  , ValidatedFrame(..)
  , TypedColumn(..)
  , CoercionWarning(..)
    -- * Typed column access
  , getNumericVec
  , getIntVec
  , getCategoricVec
  , columnName
  , columnScale
  ) where

import SCE.Core.Types
  ( DetailedError
  , ErrorCode(..)
  , Severity(..)
  , MeasurementScale(..)
  , mkError
  )
import SCE.Ingestion.Parser    (RawFrame(..))
import SCE.Ingestion.Schema    (CommittedSchema(..), CommittedColumn(..))
import SCE.Ingestion.MissingData (MissingnessReport)

import           Data.Text    (Text)
import qualified Data.Text    as T
import           Data.Vector  (Vector)
import qualified Data.Vector  as V
import           Text.Read    (readMaybe)

------------------------------------------------------------
-- Types
------------------------------------------------------------

-- | A fully typed column with its committed scale.
data TypedColumn
  = NumericCol   Text MeasurementScale (Vector Double)
  | IntegerCol   Text MeasurementScale (Vector Int)
  | CategoricCol Text MeasurementScale (Vector Text)
  deriving stock (Show)

-- | A validated, typed frame ready for statistical analysis.
data ValidatedFrame = ValidatedFrame
  { vfColumns  :: Vector TypedColumn
  , vfRowCount :: Int
  , vfSource   :: Maybe FilePath
  , vfSchema   :: CommittedSchema
  , vfMissing  :: MissingnessReport
  , vfWarnings :: [CoercionWarning]
  } deriving stock (Show)

-- | Warnings produced during coercion (do not halt processing).
data CoercionWarning
  = ValueCoercedToMissing Text Int Text  -- ^ column, row index, original value
  | HighMissingRate       Text Double    -- ^ column, missing %
  | SuspiciousOutlier     Text Int Double -- ^ column, row index, value
  deriving stock (Show)

------------------------------------------------------------
-- Typed column access
------------------------------------------------------------

columnName :: TypedColumn -> Text
columnName (NumericCol   n _ _) = n
columnName (IntegerCol   n _ _) = n
columnName (CategoricCol n _ _) = n

columnScale :: TypedColumn -> MeasurementScale
columnScale (NumericCol   _ s _) = s
columnScale (IntegerCol   _ s _) = s
columnScale (CategoricCol _ s _) = s

getNumericVec :: TypedColumn -> Maybe (Vector Double)
getNumericVec (NumericCol _ _ v)   = Just v
getNumericVec (IntegerCol _ _ v)   = Just $ V.map fromIntegral v
getNumericVec (CategoricCol _ _ _) = Nothing

getIntVec :: TypedColumn -> Maybe (Vector Int)
getIntVec (IntegerCol _ _ v)   = Just v
getIntVec _                     = Nothing

getCategoricVec :: TypedColumn -> Maybe (Vector Text)
getCategoricVec (CategoricCol _ _ v) = Just v
getCategoricVec _                     = Nothing

------------------------------------------------------------
-- Coercion
------------------------------------------------------------

-- | Apply 'CommittedSchema' to 'RawFrame', producing a 'ValidatedFrame'.
-- Unparseable values become missing (recorded as 'CoercionWarning').
-- Returns 'Left E2002' only when zero usable rows remain.
coerceFrame
  :: CommittedSchema
  -> MissingnessReport
  -> RawFrame
  -> Either DetailedError ValidatedFrame
coerceFrame schema missing rf
  | V.null (csCommittedColumns schema) =
      Left $ mkError E1004 "Cannot coerce frame: committed schema has no columns"
               ["Ensure the schema confirmation keeps at least one column."] Error
  | rfRowCount rf == 0 =
      Left $ mkError E2002 "Cannot coerce frame: RawFrame has no rows"
               ["Provide a CSV file with at least one data row."] Error
  | otherwise = do
      let committed = csCommittedColumns schema
          rows      = rfRows rf
          colNames  = rfColumns rf

      -- Coerce each committed column
      results <- V.mapM (coerceColumn colNames rows) committed
      let (typedCols, allWarnings) = V.unzip results
          warnings = concat (V.toList allWarnings)

      -- Verify we have at least one usable row.
      -- Use the length of the first non-empty column as the row count.
      -- (Columns may have different lengths if some values failed coercion.)
      let nonEmptyCols = V.filter (\c -> typedColLength c > 0) typedCols
          effectiveRows = if V.null nonEmptyCols then 0
                          else minimum $ V.toList $ V.map typedColLength nonEmptyCols

      if effectiveRows == 0
        then Left $ mkError E2002
               "Coercion produced zero usable rows"
               ["Check that the CSV data is not entirely missing or malformed."] Error
        else Right ValidatedFrame
               { vfColumns  = typedCols
               , vfRowCount = effectiveRows
               , vfSource   = rfSource rf
               , vfSchema   = schema
               , vfMissing  = missing
               , vfWarnings = warnings
               }

typedColLength :: TypedColumn -> Int
typedColLength (NumericCol   _ _ v) = V.length v
typedColLength (IntegerCol   _ _ v) = V.length v
typedColLength (CategoricCol _ _ v) = V.length v

-- | Coerce a single committed column from raw text to typed values.
coerceColumn
  :: Vector Text          -- ^ Column names in the raw frame
  -> Vector (Vector Text) -- ^ All rows
  -> CommittedColumn
  -> Either DetailedError (TypedColumn, [CoercionWarning])
coerceColumn colNames rows cc = do
  let name  = ccName cc
      scale = ccScale cc

  -- Find column index in the raw frame
  colIdx <- case V.findIndex (== name) colNames of
    Just i  -> Right i
    Nothing -> Left $ mkError E2004
                 ("Column '" <> name <> "' not found in RawFrame")
                 ["Check that column names match between schema and CSV header."] Error

  let rawVals = V.toList $ V.imap (\rowIdx row ->
                  ( rowIdx
                  , if colIdx < V.length row then row V.! colIdx else ""
                  )) rows

  case scale of
    Nominal  -> Right (coerceCategoricCol name scale rawVals)
    Ordinal  -> Right (coerceCategoricCol name scale rawVals)
    Interval -> coerceNumericCol name scale rawVals
    Ratio    -> coerceNumericCol name scale rawVals

coerceNumericCol
  :: Text
  -> MeasurementScale
  -> [(Int, Text)]
  -> Either DetailedError (TypedColumn, [CoercionWarning])
coerceNumericCol name scale rawVals =
  let parsed = map (\(i, t) ->
                 case readMaybe (T.unpack t) :: Maybe Double of
                   Just d  -> Right (i, d)
                   Nothing -> Left  (i, t))
               rawVals
      values   = [d | Right (_, d) <- parsed]
      warnings = [ValueCoercedToMissing name i t | Left (i, t) <- parsed
                                                  , not (isMissing t)]
      missWarn = if ccMissingPctFromParsed parsed > 50.0
                 then [HighMissingRate name (ccMissingPctFromParsed parsed)]
                 else []
      outlierWarns = detectOutliers name (map snd [r | Right r <- parsed])
  in if null values
       then Right (NumericCol name scale V.empty, warnings ++ missWarn)
       else Right (NumericCol name scale (V.fromList values), warnings ++ missWarn ++ outlierWarns)

coerceCategoricCol
  :: Text
  -> MeasurementScale
  -> [(Int, Text)]
  -> (TypedColumn, [CoercionWarning])
coerceCategoricCol name scale rawVals =
  let values = [t | (_, t) <- rawVals, not (isMissing t)]
  in (CategoricCol name scale (V.fromList values), [])

isMissing :: Text -> Bool
isMissing t = T.null t || T.toLower t `elem` ["na", "n/a", "null", "none", "nan", ""]

ccMissingPctFromParsed :: [Either (Int, Text) (Int, Double)] -> Double
ccMissingPctFromParsed [] = 0.0
ccMissingPctFromParsed xs =
  let missing = length [() | Left (_, t) <- xs, isMissing t]
  in fromIntegral missing / fromIntegral (length xs) * 100.0

-- | Flag values more than 3 standard deviations from the mean as suspicious.
detectOutliers :: Text -> [Double] -> [CoercionWarning]
detectOutliers _    []  = []
detectOutliers _    [_] = []
detectOutliers name vs  =
  let n    = fromIntegral (length vs) :: Double
      mean = sum vs / n
      std  = sqrt $ sum (map (\x -> (x - mean) ^ (2 :: Int)) vs) / (n - 1)
  in if std == 0.0 then []
     else [ SuspiciousOutlier name i v
          | (i, v) <- zip [0..] vs
          , abs (v - mean) > 3 * std
          ]
