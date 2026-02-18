{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}

{-|
Module      : SCE.Validation.LiquidTypes
Description : Liquid Haskell refinement types for runtime verification
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause
Maintainer  : dev@sce.example.com

This module defines refinement types using Liquid Haskell to enforce
invariants at runtime. These refinements ensure:
- Non-negative counts and indices
- Percentage bounds (0-100)
- Non-empty collections
- Finite numeric values
- Valid layout constraints
-}
module SCE.Validation.LiquidTypes
  ( -- * Basic Refinements
    validateNat
  , validatePos
  , validatePercentage
  , validateFinite
    -- * Collection Refinements
  , validateNonEmpty
  , validateValidBarCount
  , validateValidLabelLength
    -- * Layout Refinements
  , validateLayoutWidth
  , validateBarLength
    -- * Aggregate Refinements
  , validatePercentageSum
    -- * Statistical Core guards (Phase 2 / used in Phase 3)
  , validateProbability
  , validateCorrelation
  , validateDegreesOfFreedom
  , validateEffectSize
    -- * Utilities
  , isFinite
  ) where

import SCE.Core.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

-- Liquid Haskell annotations for refinement types
{-@ type Nat = {v:Int | v >= 0} @-}
{-@ type Pos = {v:Int | v > 0} @-}
{-@ type Percentage = {v:Double | 0 <= v && v <= 100} @-}
{-@ type Finite = {v:Double | isFinite v} @-}
{-@ type NonEmpty a = {v:[a] | len v > 0} @-}
{-@ type ValidBarCount = {v:Int | 1 <= v && v <= 50} @-}
{-@ type ValidLabelLength = {v:Int | 1 <= v && v <= 30} @-}
{-@ type LayoutWidth = {v:Int | 60 <= v && v <= 120} @-}
{-@ type BarLength = {v:Int | 3 <= v && v <= 100} @-}

-- | Validate that an integer is non-negative
{-@ validateNat :: Int -> Either DetailedError Nat @-}
validateNat :: Int -> SCEResult Int
validateNat n
  | n >= 0    = Right n
  | otherwise = Left (mkError E2001 "Value must be non-negative" ["Provide a non-negative integer (>= 0)."] Error)

-- | Validate that an integer is positive
{-@ validatePos :: Int -> Either DetailedError Pos @-}
validatePos :: Int -> SCEResult Int
validatePos n
  | n > 0     = Right n
  | otherwise = Left (mkError E2001 "Value must be positive" ["Provide a positive integer (> 0)."] Error)

-- | Validate that a double is a valid percentage (0-100)
{-@ validatePercentage :: Double -> Either DetailedError Percentage @-}
validatePercentage :: Double -> SCEResult Double
validatePercentage p
  | p >= 0 && p <= 100 = Right p
  | otherwise = Left (mkError E2005 ("Percentage out of range [0,100]: " <> T.pack (show p)) ["Ensure percentage values are between 0 and 100 inclusive."] Error)

-- | Validate that a double is finite (not NaN or Infinity)
{-@ validateFinite :: Double -> Either DetailedError Finite @-}
validateFinite :: Double -> SCEResult Double
validateFinite x
  | isInfinite x = Left (mkError E3004 ("Infinite value encountered: " <> T.pack (show x)) ["Check source data for division by zero or overflow."] Error)
  | isNaN x      = Left (mkError E3004 "NaN value encountered" ["Check source data for 0/0 or other indeterminate operations."] Error)
  | otherwise    = Right x

-- | Validate that a list is non-empty
{-@ validateNonEmpty :: [a] -> Either DetailedError (NonEmpty a) @-}
validateNonEmpty :: [a] -> SCEResult [a]
validateNonEmpty [] = Left (mkError E2002 "Empty collection: at least one element required" ["Provide a non-empty list."] Error)
validateNonEmpty xs = Right xs

-- | Validate bar count is within acceptable range (1-50)
{-@ validateValidBarCount :: Int -> Either DetailedError ValidBarCount @-}
validateValidBarCount :: Int -> SCEResult Int
validateValidBarCount n
  | n >= 1 && n <= 50 = Right n
  | n < 1             = Left (mkError E2002 ("Bar count too low: " <> T.pack (show n) <> ", minimum is 1") ["Add at least one data item."] Error)
  | otherwise         = Left (mkError E2008 ("Bar count too high: " <> T.pack (show n) <> ", maximum is 50") ["Reduce the number of categories or use a different chart type."] Error)

-- | Validate label length is reasonable (1-30 characters)
{-@ validateValidLabelLength :: Int -> Either DetailedError ValidLabelLength @-}
validateValidLabelLength :: Int -> SCEResult Int
validateValidLabelLength n
  | n >= 1 && n <= 30 = Right n
  | otherwise = Left (mkError E2001 "Label length must be between 1 and 30 characters" ["Shorten the label text."] Error)

-- | Validate layout width is within bounds (60-120)
{-@ validateLayoutWidth :: Int -> Either DetailedError LayoutWidth @-}
validateLayoutWidth :: Int -> SCEResult Int
validateLayoutWidth w
  | w >= 60 && w <= 120 = Right w
  | otherwise = Left (mkError E5001 "Layout width must be between 60 and 120 columns" ["Set chartWidth to a value between 60 and 120."] Error)

-- | Validate bar length is within bounds (3-100)
{-@ validateBarLength :: Int -> Either DetailedError BarLength @-}
validateBarLength :: Int -> SCEResult Int
validateBarLength l
  | l >= 3 && l <= 100 = Right l
  | otherwise = Left (mkError E5001 "Bar length must be between 3 and 100 columns" ["Set chartMaxBarLength to a value between 3 and 100."] Error)

-- | Validate that percentages sum to approximately 100
{-@ validatePercentageSum 
    :: [Percentage] 
    -> Either DetailedError {v:() | True} 
@-}
validatePercentageSum :: [Double] -> SCEResult ()
validatePercentageSum ps = do
  let total = sum ps
  let tolerance = 0.01
  if abs (total - 100.0) <= tolerance
    then Right ()
    else Left (mkError E2005 ("Percentages sum to " <> T.pack (show total) <> ", expected 100.0 (+/- " <> T.pack (show tolerance) <> ")") ["Ensure all percentages sum to 100."] Error)

-- Helper for checking if Double is finite
isFinite :: Double -> Bool
isFinite x = not (isInfinite x || isNaN x)

-- ---------------------------------------------------------------------------
-- Phase 2: Statistical Core runtime guards
-- These are plain Double -> SCEResult Double functions.
-- LiquidHaskell refinement type annotations are in comments only.
-- They will be activated as active LH syntax in Phase 6.
-- ---------------------------------------------------------------------------

-- | Validate that a value is a valid probability: in [0.0, 1.0].
-- Used by inference modules to guard p-values and power estimates.
--
-- {-@ validateProbability :: Double -> SCEResult Probability @-}  <- LH Phase 6
validateProbability :: Double -> SCEResult Double
validateProbability p
  | isNaN p || isInfinite p =
      Left (mkError E3004
              ("Probability is non-finite: " <> T.pack (show p))
              ["Check statistical computation for NaN or overflow."] Error)
  | p < 0.0 || p > 1.0 =
      Left (mkError E3004
              ("Probability out of [0, 1]: " <> T.pack (show p))
              ["Ensure the value represents a valid probability."] Error)
  | otherwise = Right p

-- | Validate that a value is a valid correlation coefficient: in [-1.0, 1.0].
-- Used by correlation modules before returning results.
--
-- {-@ validateCorrelation :: Double -> SCEResult Correlation @-}  <- LH Phase 6
validateCorrelation :: Double -> SCEResult Double
validateCorrelation r
  | isNaN r || isInfinite r =
      Left (mkError E3004
              ("Correlation is non-finite: " <> T.pack (show r))
              ["Check for zero-variance inputs or numerical instability."] Error)
  | r < (-1.0) || r > 1.0 =
      Left (mkError E3004
              ("Correlation out of [-1, 1]: " <> T.pack (show r))
              ["Correlation must lie in [-1, 1]; check the computation."] Error)
  | otherwise = Right r

-- | Validate that degrees of freedom are strictly positive.
-- Used by t-test and ANOVA modules before consulting distribution tables.
--
-- {-@ validateDegreesOfFreedom :: Double -> SCEResult Positive @-}  <- LH Phase 6
validateDegreesOfFreedom :: Double -> SCEResult Double
validateDegreesOfFreedom df
  | isNaN df || isInfinite df =
      Left (mkError E3004
              ("Degrees of freedom is non-finite: " <> T.pack (show df))
              ["Ensure sample size is sufficient for the chosen test."] Error)
  | df <= 0.0 =
      Left (mkError E4002
              ("Degrees of freedom must be > 0, got: " <> T.pack (show df))
              ["Provide a larger sample or use a test with fewer parameters."] Error)
  | otherwise = Right df

-- | Validate that an effect size is non-negative.
-- Used by effect-size modules (Cohen's d, eta-squared, etc.).
--
-- {-@ validateEffectSize :: Double -> SCEResult NonNegative @-}  <- LH Phase 6
validateEffectSize :: Double -> SCEResult Double
validateEffectSize e
  | isNaN e || isInfinite e =
      Left (mkError E3004
              ("Effect size is non-finite: " <> T.pack (show e))
              ["Check the effect size computation for numerical errors."] Error)
  | e < 0.0 =
      Left (mkError E3004
              ("Effect size must be >= 0, got: " <> T.pack (show e))
              ["Absolute-value the result if a signed effect size was intended."] Error)
  | otherwise = Right e

