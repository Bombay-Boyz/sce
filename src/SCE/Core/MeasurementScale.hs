{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}

{-|
Module      : SCE.Core.MeasurementScale
Description : Measurement scale operations and constraints
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

This module implements Stevens' measurement scale typology and enforces
which statistical operations are valid for each scale.
-}
module SCE.Core.MeasurementScale
  ( -- * Scale Operations
    canComputeMean
  , canComputeMedian
  , canComputeMode
  , canComputeRatio
    -- * Type-level Predicates
  , type SupportsOperation
  , Operation(..)
    -- * Utilities
  , inferScale
  , validateScaleOperation
  ) where

import SCE.Core.Types
import Data.Text (Text)
import qualified Data.Text as T

-- | Statistical operations
data Operation
  = ModeOp
  | MedianOp
  | MeanOp
  | RatioOp
  deriving stock (Show, Eq)

-- | Type family defining which operations are supported by each scale
type family SupportsOperation (scale :: MeasurementScale) (op :: Operation) :: Bool where
  -- Mode is supported by all scales
  SupportsOperation 'Nominal  'ModeOp   = 'True
  SupportsOperation 'Ordinal  'ModeOp   = 'True
  SupportsOperation 'Interval 'ModeOp   = 'True
  SupportsOperation 'Ratio    'ModeOp   = 'True
  
  -- Median requires at least Ordinal
  SupportsOperation 'Nominal  'MedianOp = 'False
  SupportsOperation 'Ordinal  'MedianOp = 'True
  SupportsOperation 'Interval 'MedianOp = 'True
  SupportsOperation 'Ratio    'MedianOp = 'True
  
  -- Mean requires at least Interval
  SupportsOperation 'Nominal  'MeanOp   = 'False
  SupportsOperation 'Ordinal  'MeanOp   = 'False
  SupportsOperation 'Interval 'MeanOp   = 'True
  SupportsOperation 'Ratio    'MeanOp   = 'True
  
  -- Ratio operations require Ratio scale
  SupportsOperation 'Nominal  'RatioOp  = 'False
  SupportsOperation 'Ordinal  'RatioOp  = 'False
  SupportsOperation 'Interval 'RatioOp  = 'False
  SupportsOperation 'Ratio    'RatioOp  = 'True

-- | Check if mean can be computed for a scale
canComputeMean :: MeasurementScale -> Bool
canComputeMean Nominal  = False
canComputeMean Ordinal  = False
canComputeMean Interval = True
canComputeMean Ratio    = True

-- | Check if median can be computed for a scale
canComputeMedian :: MeasurementScale -> Bool
canComputeMedian Nominal = False
canComputeMedian Ordinal = True
canComputeMedian Interval = True
canComputeMedian Ratio = True

-- | Check if mode can be computed for a scale
canComputeMode :: MeasurementScale -> Bool
canComputeMode _ = True  -- All scales support mode

-- | Check if ratio operations can be performed
canComputeRatio :: MeasurementScale -> Bool
canComputeRatio Ratio = True
canComputeRatio _     = False

-- | Infer measurement scale from data characteristics
inferScale :: [DataValue] -> MeasurementScale
inferScale values
  | all isNumeric values && any hasNegative values = Interval
  | all isNumeric values = Ratio
  | all isOrdered values = Ordinal
  | otherwise = Nominal
  where
    isNumeric (NumericValue _) = True
    isNumeric (IntegerValue _) = True
    isNumeric _ = False
    
    hasNegative (NumericValue x) = x < 0
    hasNegative (IntegerValue x) = x < 0
    hasNegative _ = False
    
    isOrdered (TextValue t) = T.all (`elem` ("0123456789" :: String)) t
    isOrdered _ = True

-- | Validate that an operation is permitted for a scale
validateScaleOperation :: MeasurementScale -> Operation -> SCEResult ()
validateScaleOperation scale op = 
  if isSupported scale op
    then Right ()
    else Left $ mkError E2003
      ("Operation " <> T.pack (show op) <>
       " not supported for " <> T.pack (show scale) <> " scale")
      ["Choose an operation valid for " <> T.pack (show scale) <> " scale."]
      Error
  where
    isSupported Nominal  ModeOp   = True
    isSupported Ordinal  ModeOp   = True
    isSupported Interval ModeOp   = True
    isSupported Ratio    ModeOp   = True
    
    isSupported Nominal  MedianOp = False
    isSupported Ordinal  MedianOp = True
    isSupported Interval MedianOp = True
    isSupported Ratio    MedianOp = True
    
    isSupported Nominal  MeanOp   = False
    isSupported Ordinal  MeanOp   = False
    isSupported Interval MeanOp   = True
    isSupported Ratio    MeanOp   = True
    
    isSupported Nominal  RatioOp  = False
    isSupported Ordinal  RatioOp  = False
    isSupported Interval RatioOp  = False
    isSupported Ratio    RatioOp  = True
    
    isSupported _ _ = False
