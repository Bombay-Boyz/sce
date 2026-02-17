{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData         #-}

{-|
Module      : SCE.Ingestion.MissingData
Description : Missing-data analysis and complete-case filtering
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Analyses missingness patterns in a 'RawFrame' and provides a complete-case
filter that removes rows with any missing values in the specified columns.
-}
module SCE.Ingestion.MissingData
  ( -- * Analysis
    analyseMissingness
  , completeCaseFilter
    -- * Types
  , MissingnessReport(..)
  , MissingnessPattern(..)
  , ColumnMissingness(..)
  ) where

import SCE.Core.Types
  ( DetailedError
  , ErrorCode(..)
  , Severity(..)
  , mkError
  )
import SCE.Ingestion.Parser (RawFrame(..), RawRow)

import           Data.Text    (Text)
import qualified Data.Text    as T
import           Data.Vector  (Vector)
import qualified Data.Vector  as V
import           Data.List    (findIndices)

------------------------------------------------------------
-- Types
------------------------------------------------------------

-- | Heuristic classification of a column's missingness mechanism.
data MissingnessPattern
  = CompleteData        -- ^ No missing values in this column
  | MCAR                -- ^ Missing Completely At Random (heuristic: random scatter)
  | MAR                 -- ^ Missing At Random (heuristic: missing in clusters)
  | MNAR                -- ^ Missing Not At Random (heuristic: high % missing)
  | UnknownMissingness  -- ^ Cannot classify with available information
  deriving stock (Show, Eq)

-- | Missingness statistics for a single column.
data ColumnMissingness = ColumnMissingness
  { cmColumn     :: Text
  , cmMissingN   :: Int
  , cmMissingPct :: Double
  , cmPattern    :: MissingnessPattern
  , cmRows       :: [Int]   -- ^ Row indices (0-based) with missing values, capped at 20
  } deriving stock (Show)

-- | Missingness summary for an entire frame.
data MissingnessReport = MissingnessReport
  { mrColumns        :: [ColumnMissingness]
  , mrCompleteRows   :: Int   -- ^ Rows with no missing values in any column
  , mrAnyMissingRows :: Int   -- ^ Rows with at least one missing value
  , mrRecommendation :: Text
  } deriving stock (Show)

------------------------------------------------------------
-- Analysis
------------------------------------------------------------

-- | Compute full missingness analysis for a 'RawFrame'. Pure.
analyseMissingness :: RawFrame -> MissingnessReport
analyseMissingness rf =
  let cols       = V.toList (rfColumns rf)
      rows       = rfRows rf
      nRows      = rfRowCount rf
      colMiss    = zipWith (\i col -> analyseColumn i col rows nRows)
                           [0..] cols
      rowMissing = V.map (rowHasMissing (V.length (rfColumns rf))) rows
      anyMiss    = V.length $ V.filter id rowMissing
      complete   = nRows - anyMiss
      rec        = buildRecommendation complete nRows colMiss
  in MissingnessReport
       { mrColumns        = colMiss
       , mrCompleteRows   = complete
       , mrAnyMissingRows = anyMiss
       , mrRecommendation = rec
       }

analyseColumn :: Int -> Text -> Vector RawRow -> Int -> ColumnMissingness
analyseColumn colIdx colName rows totalRows =
  let missingIndices = findIndices (\row ->
                         let val = if colIdx < V.length row then row V.! colIdx else ""
                         in isMissing val)
                       (V.toList rows)
      missingN   = length missingIndices
      missingPct = if totalRows == 0 then 0.0
                   else fromIntegral missingN / fromIntegral totalRows * 100.0
      pattern_   = classifyPattern missingN missingPct missingIndices totalRows
  in ColumnMissingness
       { cmColumn     = colName
       , cmMissingN   = missingN
       , cmMissingPct = missingPct
       , cmPattern    = pattern_
       , cmRows       = take 20 missingIndices
       }

-- | Heuristic missingness pattern classification.
classifyPattern :: Int -> Double -> [Int] -> Int -> MissingnessPattern
classifyPattern 0    _   _       _     = CompleteData
classifyPattern miss pct indices total
  | pct >= 80.0             = MNAR   -- Very high rate → likely systematic
  | isClusteredMissing indices total = MAR   -- Missing in blocks → correlated
  | otherwise               = MCAR  -- Scattered → random
  where
    -- Heuristic: missingness is clustered if consecutive gaps are all small
    isClusteredMissing []          _ = False
    isClusteredMissing [_]         _ = False
    isClusteredMissing (i1:i2:rest) n =
      let allIdxs  = i1 : i2 : rest
          gaps     = zipWith (-) (i2:rest) allIdxs
          maxGap   = foldl max 0 gaps
      in maxGap < max 2 (n `div` 10)

rowHasMissing :: Int -> RawRow -> Bool
rowHasMissing nCols row =
  V.length row < nCols
  || V.any isMissing row

isMissing :: Text -> Bool
isMissing t = T.null t || T.toLower t `elem` ["na", "n/a", "null", "none", "nan", ""]

buildRecommendation :: Int -> Int -> [ColumnMissingness] -> Text
buildRecommendation complete total cols
  | complete == total = "Data is complete; no missing value handling required."
  | complete == 0     = "No complete rows. Consider imputation or relaxing column requirements."
  | pct >= 50.0       = "More than half of rows have missing values. Consider imputation."
  | otherwise         = "Some rows have missing values. Complete-case analysis will retain "
                          <> T.pack (show complete) <> " of " <> T.pack (show total) <> " rows."
  where
    pct :: Double
    pct = fromIntegral (total - complete) / fromIntegral total * 100.0

------------------------------------------------------------
-- Complete-case filtering
------------------------------------------------------------

-- | Return a 'RawFrame' containing only rows with no missing values in the
-- specified columns. If the column list is empty, all columns are checked.
-- Returns 'Left E2002' if filtering would leave zero rows.
completeCaseFilter :: [Text] -> RawFrame -> Either DetailedError RawFrame
completeCaseFilter targetCols rf =
  let cols    = rfColumns rf
      indices = if null targetCols
                  then [0 .. V.length cols - 1]
                  else concatMap (\c -> V.toList $ V.findIndices (== c) cols) targetCols
      rows    = rfRows rf
      kept    = V.filter (rowComplete indices) rows
  in if V.null kept
       then Left $ mkError E2002
              "completeCaseFilter: no rows remain after removing rows with missing values"
              [ "Consider imputation instead of complete-case analysis."
              , "Check that the target columns actually contain data."
              ] Error
       else Right rf
              { rfRows     = kept
              , rfRowCount = V.length kept
              }

rowComplete :: [Int] -> RawRow -> Bool
rowComplete indices row =
  all (\i -> i < V.length row && not (isMissing (row V.! i))) indices
