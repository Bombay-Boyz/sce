{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SCE.Validation.Validator
Description : Data validation and constraint checking
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Validates data integrity and enforces business rules.
-}
module SCE.Validation.Validator
  ( -- * Validation Functions
    validateDataTable
  , validateChartData
  , validateNumericColumn
  , validatePercentages
  ) where

import SCE.Core.Types
import SCE.Core.DataModel
import SCE.Core.MeasurementScale
import SCE.Validation.LiquidTypes
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)

-- | Validate a complete data table
validateDataTable :: DataTable -> SCEResult ()
validateDataTable table = do
  -- Check non-empty
  _ <- validatePos (V.length $ tableRows table)
  _ <- validatePos (V.length $ tableColumns table)
  
  -- Validate each numeric column
  mapM_ (validateNumericColumnInTable table) (V.toList $ tableColumns table)
  
  return ()

-- | Validate a numeric column in a table
validateNumericColumnInTable :: DataTable -> ColumnName -> SCEResult ()
validateNumericColumnInTable table colName = do
  case getNumericColumn colName table of
    Right values -> validateNumericColumn values
    Left _ -> return ()  -- Skip non-numeric columns

-- | Validate a numeric column
validateNumericColumn :: Vector Double -> SCEResult ()
validateNumericColumn values = do
  -- Check all values are finite
  V.mapM_ validateFinite values
  return ()

-- | Validate chart data before generation
validateChartData :: ChartConfig -> Vector (Text, Double) -> SCEResult ()
validateChartData config items = do
  -- Validate bar count
  _ <- validateValidBarCount (V.length items)
  
  -- Validate layout width
  _ <- validateLayoutWidth (chartWidth config)
  
  -- Validate bar length
  _ <- validateBarLength (chartMaxBarLength config)
  
  -- Validate all values are finite and non-negative
  V.mapM_ (validateFinite . snd) items
  
  return ()

-- | Validate percentages sum to 100
validatePercentages :: Vector Double -> SCEResult ()
validatePercentages percentages = do
  -- Validate each percentage is in valid range
  V.mapM_ validatePercentage percentages
  
  -- Validate sum
  validatePercentageSum (V.toList percentages)
