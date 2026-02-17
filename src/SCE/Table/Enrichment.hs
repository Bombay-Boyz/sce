{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SCE.Table.Enrichment
Description : Automatic table enrichment with derived metrics
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

This module implements automatic enrichment of data tables with:
- Totals and subtotals
- Percentages of total
- Cumulative values
- Delta calculations (absolute and percentage)
- Summary statistics
-}
module SCE.Table.Enrichment
  ( -- * Enrichment Functions
    enrichTable
  , addTotals
  , addPercentages
  , addCumulative
  , addDeltas
  , EnrichedTable(..)
  , Enrichment(..)
  ) where

import SCE.Core.Types
import SCE.Core.Statistics
import SCE.Core.DataModel
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad (foldM)

-- | Types of enrichment applied
data Enrichment
  = TotalsEnrichment
  | PercentagesEnrichment
  | CumulativeEnrichment
  | DeltaEnrichment
  | SummaryStatsEnrichment
  deriving stock (Show, Eq)

-- | Enriched table with additional computed columns
data EnrichedTable = EnrichedTable
  { enrichedOriginal     :: DataTable
  , enrichedColumns      :: Vector ColumnName
  , enrichedRows         :: Vector DataRow
  , enrichedTotals       :: Maybe (Map ColumnName DataValue)
  , enrichedSummaryStats :: Maybe (Map ColumnName SummaryStats)
  , enrichedApplied      :: [Enrichment]
  }
  deriving stock (Show)

-- | Enrich a table with all applicable enrichments
enrichTable :: DataTable -> SCEResult EnrichedTable
enrichTable table = do
  -- Add percentage columns for numeric columns
  tableWithPct <- addPercentages table
  
  -- Add cumulative columns
  tableWithCum <- addCumulative tableWithPct
  
  -- Add delta columns
  tableWithDelta <- addDeltas tableWithCum
  
  -- Add CAGR if applicable
  tableWithCAGR <- addCAGR tableWithDelta
  
  -- Compute totals
  totals <- computeTotals tableWithCAGR
  
  -- Compute summary statistics
  summaryStats <- computeAllSummaryStats tableWithCAGR
  
  return EnrichedTable
    { enrichedOriginal = table
    , enrichedColumns = tableColumns tableWithCAGR
    , enrichedRows = tableRows tableWithCAGR
    , enrichedTotals = Just totals
    , enrichedSummaryStats = Just summaryStats
    , enrichedApplied = 
        [ TotalsEnrichment
        , PercentagesEnrichment
        , CumulativeEnrichment
        , DeltaEnrichment
        , SummaryStatsEnrichment
        ]
    }

-- | Add percentage of total columns
addPercentages :: DataTable -> SCEResult DataTable
addPercentages table = do
  let numericCols = getNumericColumns table
  percentageCols <- mapM (addPercentageColumn table) numericCols
  return $ foldl mergeColumn table percentageCols
  where
    getNumericColumns :: DataTable -> [ColumnName]
    getNumericColumns tbl = 
      V.toList $ V.filter (isNumericColumn tbl) (tableColumns tbl)
    
    isNumericColumn :: DataTable -> ColumnName -> Bool
    isNumericColumn tbl col = 
      case getNumericColumn col tbl of
        Right _ -> True
        Left _ -> False

-- | Add percentage column for a numeric column
addPercentageColumn :: DataTable -> ColumnName -> SCEResult DataTable
addPercentageColumn table colName = do
  values <- getNumericColumn colName table
  let total = V.sum values
  if total == 0
    then Left (mkError E3004 "Cannot compute percentages: column total is zero" ["Ensure the numeric column has a non-zero sum."] Error)
    else do
      let percentages = V.map (\v -> (v / total) * 100.0) values
      let pctColName = colName <> " (%)"
      let newRows = V.zipWith (addValueToRow pctColName) 
                              (tableRows table) 
                              (V.map NumericValue percentages)
      return table
        { tableColumns = tableColumns table `V.snoc` pctColName
        , tableRows = newRows
        }

-- | Add cumulative sum columns
addCumulative :: DataTable -> SCEResult DataTable
addCumulative table = do
  let numericCols = V.filter (isNumericColumn table) (tableColumns table)
  foldM addCumulativeColumn table (V.toList numericCols)
  where
    isNumericColumn tbl col = 
      case getNumericColumn col tbl of
        Right _ -> True
        Left _ -> False

-- | Add cumulative column for a numeric column
addCumulativeColumn :: DataTable -> ColumnName -> SCEResult DataTable
addCumulativeColumn table colName = do
  values <- getNumericColumn colName table
  let cumulative = computeCumulative values
  let cumColName = colName <> " (Cumulative)"
  let newRows = V.zipWith (addValueToRow cumColName) 
                          (tableRows table)
                          (V.map NumericValue cumulative)
  return table
    { tableColumns = tableColumns table `V.snoc` cumColName
    , tableRows = newRows
    }

-- | Add delta (change) columns
addDeltas :: DataTable -> SCEResult DataTable
addDeltas table = do
  let numericCols = V.filter (isNumericColumn table) (tableColumns table)
  foldM addDeltaColumn table (V.toList numericCols)
  where
    isNumericColumn tbl col = 
      case getNumericColumn col tbl of
        Right _ -> True
        Left _ -> False

-- | Add delta column for a numeric column
addDeltaColumn :: DataTable -> ColumnName -> SCEResult DataTable
addDeltaColumn table colName = do
  values <- getNumericColumn colName table
  if V.length values < 2
    then return table
    else do
      let deltas = V.cons 0 $ computeDelta values
      let deltaColName = colName <> " (Δ)"
      let newRows = V.zipWith (addValueToRow deltaColName)
                              (tableRows table)
                              (V.map NumericValue deltas)
      return table
        { tableColumns = tableColumns table `V.snoc` deltaColName
        , tableRows = newRows
        }

-- | Add totals row
addTotals :: DataTable -> SCEResult (Map ColumnName DataValue)
addTotals = computeTotals

-- | Add CAGR (Compound Annual Growth Rate) columns if applicable
-- CAGR is applicable when:
-- 1. There's a time column (year, period, etc.)
-- 2. There's at least one numeric column
-- 3. Time periods are sequential (at least 2 periods)
addCAGR :: DataTable -> SCEResult DataTable
addCAGR table = do
  let cols = tableColumns table
  
  -- Detect time column
  case V.find isTimeColumn cols of
    Nothing -> Right table  -- No time column, skip CAGR
    Just timeCol -> do
      -- Get time values to determine period count
      case getColumn timeCol table of
        Left _ -> Right table  -- Can't read time column, skip
        Right timeValues -> do
          let numPeriods = V.length timeValues
          
          -- Need at least 2 periods for CAGR
          if numPeriods < 2
            then Right table
            else do
              -- Add CAGR for each numeric column
              let numericCols = V.filter (\col -> col /= timeCol && isNumericCol col) cols
              if V.null numericCols
                then Right table
                else
                  -- For each numeric column, add a CAGR column
                  foldM (addCAGRColumn numPeriods) table (V.toList numericCols)
  where
    isNumericCol col = case getNumericColumn col table of
      Right _ -> True
      Left _ -> False
    
    isTimeColumn name =
      let lower = T.toLower name
      in any (`T.isInfixOf` lower) ["year", "period", "quarter", "month", "date", "time"]

-- | Add CAGR column for a specific numeric column
addCAGRColumn :: Int -> DataTable -> ColumnName -> SCEResult DataTable
addCAGRColumn numPeriods table colName = do
  values <- getNumericColumn colName table
  
  case (V.uncons values, V.unsnoc values) of
    (Just (startVal, _), Just (_, endVal)) -> do
      -- Calculate CAGR: ((endValue / startValue)^(1/numPeriods)) - 1
      let cagr = if startVal > 0 && endVal > 0
                 then ((endVal / startVal) ** (1 / fromIntegral (numPeriods - 1))) - 1
                 else 0  -- Can't calculate CAGR for zero or negative values
      
      let cagrPercent = cagr * 100
      let cagrCol = colName <> "_CAGR"
      let cagrValue = NumericValue cagrPercent
      
      -- Add CAGR as a new column (same value for all rows - it's a summary statistic)
      let newRows = V.map (\row -> M.insert cagrCol cagrValue row) (tableRows table)
      let newCols = if V.elem cagrCol (tableColumns table)
                    then tableColumns table
                    else tableColumns table `V.snoc` cagrCol
      let newScales = M.insert cagrCol Ratio (tableScales table)
      
      Right table
        { tableColumns = newCols
        , tableRows = newRows
        , tableScales = newScales
        }
    
    _ -> Right table  -- Not enough data for CAGR

------------------------------------------------------------
-- Total and Statistics Computation
------------------------------------------------------------

-- | Compute totals for all numeric columns
computeTotals :: DataTable -> SCEResult (Map ColumnName DataValue)
computeTotals table = do
  let numericCols = V.filter (isNumericColumn table) (tableColumns table)
  totalsMap <- foldM computeColumnTotal M.empty (V.toList numericCols)
  return totalsMap
  where
    isNumericColumn tbl col = 
      case getNumericColumn col tbl of
        Right _ -> True
        Left _ -> False
    
    computeColumnTotal :: Map ColumnName DataValue -> ColumnName 
                       -> SCEResult (Map ColumnName DataValue)
    computeColumnTotal acc colName = do
      values <- getNumericColumn colName table
      let total = V.sum values
      return $ M.insert colName (NumericValue total) acc

-- | Compute summary statistics for all numeric columns
computeAllSummaryStats :: DataTable -> SCEResult (Map ColumnName SummaryStats)
computeAllSummaryStats table = do
  let numericCols = V.filter (isNumericColumn table) (tableColumns table)
  foldM computeColumnStats M.empty (V.toList numericCols)
  where
    isNumericColumn tbl col = 
      case getNumericColumn col tbl of
        Right _ -> True
        Left _ -> False
    
    computeColumnStats :: Map ColumnName SummaryStats -> ColumnName
                       -> SCEResult (Map ColumnName SummaryStats)
    computeColumnStats acc colName = do
      values <- getNumericColumn colName table
      scale <- case M.lookup colName (tableScales table) of
        Just s -> Right s
        Nothing -> Right Ratio  -- Default to Ratio if not found
      stats <- computeSummaryStats scale values
      return $ M.insert colName stats acc

-- Helper functions

addValueToRow :: ColumnName -> DataRow -> DataValue -> DataRow
addValueToRow colName row value = M.insert colName value row

-- | Merge columns from two tables
-- Combines columns from both tables, with new table's columns added to base
-- Rows are merged using left-biased union (base values take precedence)
mergeColumn :: DataTable -> DataTable -> DataTable
mergeColumn base new =
  let newCols = V.filter (\col -> not $ V.elem col (tableColumns base)) (tableColumns new)
      mergedColumns = tableColumns base V.++ newCols
      mergedRows = V.zipWith M.union (tableRows base) (tableRows new)
      mergedScales = M.union (tableScales base) (tableScales new)
  in base 
      { tableColumns = mergedColumns
      , tableRows = mergedRows
      , tableScales = mergedScales
      }
