{-|
Module      : SCE.Liquid.Refinements
Description : LiquidHaskell refinement type aliases (Phase 6 stub)
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Documentation stub — LiquidHaskell annotations activated in Phase 6.

This module establishes the module path and records the planned refinement
type aliases. No '{-# LANGUAGE LiquidHaskell #-}' pragma is present and
'liquidhaskell' is NOT a build dependency. The annotations below are
block comments only; they will become active LH syntax in Phase 6.

Planned refinements (activate in Phase 6):

  {-@ type Probability   = {v:Double | 0.0 <= v && v <= 1.0}  @-}
  {-@ type Positive      = {v:Double | v > 0.0}               @-}
  {-@ type NonNegative   = {v:Double | v >= 0.0}              @-}
  {-@ type Correlation   = {v:Double | -1.0 <= v && v <= 1.0} @-}
  {-@ type NonEmptyVec a  = {v:V.Vector a | vLen v > 0}       @-}
  {-@ type MinSizeVec a N = {v:V.Vector a | vLen v >= N}      @-}
  {-@ type PValue        = {v:Double | 0.0 <= v && v <= 1.0}  @-}
  {-@ type CIBounds      = {v:(Double,Double) | fst v <= snd v} @-}
-}
module SCE.Liquid.Refinements where
-- LiquidHaskell annotations activated in Phase 6.
-- No runtime content; this module exists to reserve the module path.
