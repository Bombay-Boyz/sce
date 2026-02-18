{-|
Module      : SCE.Liquid.Properties
Description : LiquidHaskell property specifications (Phase 6 stub)
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Documentation stub — LiquidHaskell annotations activated in Phase 6.

Records the mathematical properties to be formally verified in Phase 6.
These are the LH-verified counterparts of the QuickCheck properties
already tested in 'Property.DescriptiveProperties'.

Planned property annotations (activate in Phase 6):

  -- Mean is bounded by the min and max of its input.
  {-@ meanBounded :: xs:NonEmptyVec Double
                  -> {v:Double | v >= vMin xs && v <= vMax xs} @-}
  meanBounded :: V.Vector Double -> Double

  -- Sample variance is always non-negative.
  {-@ varianceNonNeg :: xs:MinSizeVec Double 2 -> NonNegative @-}
  varianceNonNeg :: V.Vector Double -> Double

  -- Standard deviation is always non-negative.
  {-@ stdDevNonNeg :: xs:MinSizeVec Double 2 -> NonNegative @-}
  stdDevNonNeg :: V.Vector Double -> Double

  -- IQR is always non-negative (Q3 >= Q1 after sorting).
  {-@ iqrNonNeg :: xs:NonEmptyVec Double -> NonNegative @-}
  iqrNonNeg :: V.Vector Double -> Double

  -- Pearson correlation is always in [-1, 1].
  {-@ pearsonBounded :: xs:MinSizeVec Double 3
                     -> ys:{v:V.Vector Double | vLen v = vLen xs}
                     -> Correlation @-}
  pearsonBounded :: V.Vector Double -> V.Vector Double -> Double

  -- p-values are always in [0, 1].
  {-@ pValueBounded :: Double -> PValue @-}
  pValueBounded :: Double -> Double

  -- CI lower bound <= upper bound.
  {-@ ciBoundsOrdered :: ConfidenceInterval
                      -> {v:Bool | v = True} @-}
  ciBoundsOrdered :: ConfidenceInterval -> Bool

Acceptance criteria for Phase 6:
  - liquid src/SCE/Liquid/Properties.hs exits 0
  - meanBounded verified: mean in [vMin, vMax] for all inputs
  - varianceNonNeg verified: variance >= 0 for n >= 2
  - stdDevNonNeg verified: stddev >= 0 for n >= 2
  - pearsonBounded verified: correlation in [-1, 1]
  - pValueBounded verified: p-value in [0, 1]
  - ciBoundsOrdered verified: lower <= upper
-}
module SCE.Liquid.Properties where
-- LiquidHaskell annotations activated in Phase 6.
-- No runtime content; this module exists to reserve the module path.
