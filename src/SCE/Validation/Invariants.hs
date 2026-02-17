{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SCE.Validation.Invariants
Description : Cross-column invariants and business rules
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Enforces complex invariants across multiple columns.
-}
module SCE.Validation.Invariants
  ( -- * Invariant Checks
    checkPercentageInvariants
  , checkTimeSeriesInvariants
  , checkRangeInvariants
  ) where

import SCE.Core.Types
import SCE.Core.DataModel
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Check percentage column invariants
checkPercentageInvariants :: Vector Double -> SCEResult ()
checkPercentageInvariants percentages = do
  -- Each percentage must be 0-100
  V.mapM_ checkRange percentages
  
  -- Sum should be approximately 100
  let total = V.sum percentages
  if abs (total - 100.0) <= 0.01
    then Right ()
    else Left (mkError E2005 ("Percentage sum is " <> T.pack (show total) <> ", expected 100.0 (+/- 0.01)") ["Ensure all percentages sum to 100."] Error)
  where
    checkRange p = 
      if p >= 0 && p <= 100
        then Right ()
        else Left (mkError E2005 ("Percentage out of range [0,100]: " <> T.pack (show p)) ["Ensure all percentage values are between 0 and 100."] Error)

-- | Check time series invariants
checkTimeSeriesInvariants :: Vector Double -> SCEResult ()
checkTimeSeriesInvariants values
  | V.null values = Left (mkError E2002 "Time series is empty: at least 2 values required" ["Provide at least 2 time points."] Error)
  | V.length values < 2 = Left (mkError E2002 ("Time series has " <> T.pack (show (V.length values)) <> " point(s), need at least 2") ["Provide at least 2 time points."] Error)
  | otherwise = Right ()

-- | Check range invariants
checkRangeInvariants :: Double -> Double -> Vector Double -> SCEResult ()
checkRangeInvariants minVal maxVal values =
  V.mapM_ checkInRange values
  where
    checkInRange v =
      if v >= minVal && v <= maxVal
        then Right ()
        else Left (mkError E2001 ("Value " <> T.pack (show v) <> " is outside range [" <> T.pack (show minVal) <> ", " <> T.pack (show maxVal) <> "]") ["Ensure all values lie within the expected range."] Error)
