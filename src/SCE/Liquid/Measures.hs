{-|
Module      : SCE.Liquid.Measures
Description : LiquidHaskell measure definitions (Phase 6 stub)
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Documentation stub — LiquidHaskell annotations activated in Phase 6.

Measures are logical functions over Haskell types that LiquidHaskell uses
in refinement predicates. They are NOT regular Haskell functions — they
live in the LH specification layer only.

Planned measures (activate in Phase 6):

  {-@ measure vLen :: V.Vector a -> Int
      vLen = Data.Vector.length                         @-}

  {-@ measure vMin :: V.Vector Double -> Double
      vMin xs = Data.Vector.minimum xs                 @-}

  {-@ measure vMax :: V.Vector Double -> Double
      vMax xs = Data.Vector.maximum xs                 @-}

  {-@ measure isNonNeg :: Double -> Bool
      isNonNeg x = x >= 0.0                            @-}

  {-@ measure isProb :: Double -> Bool
      isProb x = 0.0 <= x && x <= 1.0                 @-}

  {-@ measure isCorr :: Double -> Bool
      isCorr x = -1.0 <= x && x <= 1.0                @-}
-}
module SCE.Liquid.Measures where
-- LiquidHaskell annotations activated in Phase 6.
-- No runtime content; this module exists to reserve the module path.
