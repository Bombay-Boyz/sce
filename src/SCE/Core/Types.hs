{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

{-|
Module      : SCE.Core.Types
Description : Core type definitions for the Statistical Charting Engine
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause
Maintainer  : dev@sce.example.com

Phase 0 migration: ValidationError / SCEResult replaced by
DetailedError / SCEResult (from SCE.Core.Error).
-}
module SCE.Core.Types
  ( -- * Measurement Scales
    MeasurementScale(..)
  , Measurement(..)
  , getMeasurementValue
  , getMeasurementScale
    -- * Data Types
  , DataValue(..)
  , DataRow
  , DataTable(..)
  , ColumnName
  , TableMetadata(..)
    -- * Result / Error types (re-exported from SCE.Core.Error)
  , DetailedError(..)
  , ErrorCode(..)
  , ErrorContext(..)
  , Severity(..)
  , SCEResult
  , mkError
  , mkErrorAt
  , addContext
  , addSuggestion
  , formatError
  , formatErrorShort
    -- * Chart Types
  , ChartType(..)
  , ChartConfig(..)
  , RejectionReason(..)
    -- * Output Formats
  , OutputFormat(..)
  , Default(..)
  ) where

import Data.Text (Text)
import Data.Map.Strict (Map)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import SCE.Core.Error
  ( DetailedError(..)
  , ErrorCode(..)
  , ErrorContext(..)
  , Severity(..)
  , SCEResult
  , mkError
  , mkErrorAt
  , addContext
  , addSuggestion
  , formatError
  , formatErrorShort
  )

-- ---------------------------------------------------------------------------
-- Measurement Scales
-- ---------------------------------------------------------------------------

data MeasurementScale
  = Nominal
  | Ordinal
  | Interval
  | Ratio
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

data Measurement (scale :: MeasurementScale) where
  NominalMeasurement  :: Text   -> Measurement 'Nominal
  OrdinalMeasurement  :: Int -> Text -> Measurement 'Ordinal
  IntervalMeasurement :: Double -> Measurement 'Interval
  RatioMeasurement    :: Double -> Measurement 'Ratio

deriving stock instance Show (Measurement scale)
deriving stock instance Eq   (Measurement scale)

getMeasurementValue :: Measurement scale -> Maybe Double
getMeasurementValue (NominalMeasurement _)      = Nothing
getMeasurementValue (OrdinalMeasurement rank _) = Just (fromIntegral rank)
getMeasurementValue (IntervalMeasurement v)     = Just v
getMeasurementValue (RatioMeasurement v)        = Just v

getMeasurementScale :: Measurement scale -> MeasurementScale
getMeasurementScale (NominalMeasurement _)   = Nominal
getMeasurementScale (OrdinalMeasurement _ _) = Ordinal
getMeasurementScale (IntervalMeasurement _)  = Interval
getMeasurementScale (RatioMeasurement _)     = Ratio

-- ---------------------------------------------------------------------------
-- Data Model
-- ---------------------------------------------------------------------------

data DataValue
  = TextValue    Text
  | NumericValue Double
  | IntegerValue Int
  | MissingValue
  deriving stock (Show, Eq, Generic)

type ColumnName = Text
type DataRow    = Map ColumnName DataValue

data DataTable = DataTable
  { tableMetadata :: TableMetadata
  , tableColumns  :: Vector ColumnName
  , tableRows     :: Vector DataRow
  , tableScales   :: Map ColumnName MeasurementScale
  } deriving stock (Show, Generic)

data TableMetadata = TableMetadata
  { metaTitle       :: Maybe Text
  , metaSource      :: Maybe FilePath
  , metaDescription :: Maybe Text
  , metaRowCount    :: Int
  , metaColCount    :: Int
  } deriving stock (Show, Eq, Generic)

-- ---------------------------------------------------------------------------
-- Chart Types
-- ---------------------------------------------------------------------------

data ChartType
  = HorizontalBarChart
  | VerticalBarChart
  | LineChart
  | ScatterPlot
  | DotPlot
  | Histogram
  | BoxPlot
  | ConfidenceInterval
  deriving stock (Show, Eq, Enum, Bounded, Generic)

data ChartConfig = ChartConfig
  { chartType         :: ChartType
  , chartWidth        :: Int
  , chartMaxBarLength :: Int
  , chartFillChar     :: Char
  , chartShowValues   :: Bool
  , chartShowScale    :: Bool
  } deriving stock (Show, Eq, Generic)

instance Default ChartConfig where
  def = ChartConfig
    { chartType         = HorizontalBarChart
    , chartWidth        = 80
    , chartMaxBarLength = 50
    , chartFillChar     = '='
    , chartShowValues   = True
    , chartShowScale    = True
    }

data RejectionReason
  = TooManyCategoriesRejection Int Int
  | InsufficientDataRejection  Int Int
  | InvalidScaleRejection      MeasurementScale ChartType
  | DataQualityRejection       Text
  | TightlyClusteredRejection  Double Double Double
  deriving stock (Show, Eq, Generic)

-- ---------------------------------------------------------------------------
-- Output Formats
-- ---------------------------------------------------------------------------

data OutputFormat
  = PlainText
  | Markdown
  | DocX
  deriving stock (Show, Eq, Enum, Bounded, Generic)

class Default a where
  def :: a
