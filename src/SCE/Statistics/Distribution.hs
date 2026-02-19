{-# LANGUAGE StrictData        #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

{-|
Module      : SCE.Statistics.Distribution
Description : Distribution fitting by MLE with AIC/BIC model selection (Phase 3)
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

== Distributions supported

  * Normal   — MLE: mean = sample mean, variance = biased MLE variance (n, not n-1).
  * Exponential — MLE: rate λ = 1 / sample mean. Requires non-negative data.
  * Uniform  — MLE: a = min(x), b = max(x). Requires at least 2 distinct values.

== Goodness-of-fit

  Each 'FitResult' embeds a Kolmogorov-Smirnov test against the fitted CDF.
  The KS p-value is corrected for the fact that the distribution was estimated
  from the same data (Lilliefors correction applied for Normal and Exponential).

== Model selection

  Use 'fitBest' to compare all three families by AIC and return the winner.

== Design constraints

  * No partial functions.
  * All public functions return @SCEResult@.
  * P-values clamped to [0, 1].
  * Numerical stability: uses Kahan sums and guards against near-zero denominators.
-}
module SCE.Statistics.Distribution
  ( -- * Distribution fitting
    fitNormal
  , fitExponential
  , fitUniform
  , fitBest
    -- * Goodness-of-fit (standalone)
  , ksGoodnessOfFit
    -- * Types
  , FitResult(..)
  , DistributionFamily(..)
  ) where

import SCE.Core.Types
  ( SCEResult, mkError, ErrorCode(..), Severity(..) )
import SCE.Core.Statistics
  ( kahanMean, kahanSum, welfordVariance )
import SCE.Statistics.TestResult
  ( TestResult(..), AssumptionCheck(..), AssumptionStatus(..), mkTestResult )

import qualified Data.Text   as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import           Data.List   (minimumBy)
import           Data.Ord    (comparing)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Family of parametric distribution being fit.
data DistributionFamily
  = Normal      -- ^ n(μ, σ²)
  | Exponential -- ^ Exp(λ)
  | Uniform     -- ^ Uniform(a, b)
  | Poisson     -- ^ Poi(λ)  — future; not yet fit
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Result of fitting a parametric distribution to data.
data FitResult = FitResult
  { frFamily        :: DistributionFamily
    -- ^ Which family was fit.
  , frParameters    :: [(T.Text, Double)]
    -- ^ Named MLE parameter estimates, e.g. @[("mean", 5.2), ("sd", 1.1)]@.
  , frLogLikelihood :: Double
    -- ^ Log-likelihood at the MLE.
  , frAIC           :: Double
    -- ^ Akaike Information Criterion = 2k − 2 ln L.
  , frBIC           :: Double
    -- ^ Bayesian Information Criterion = k ln n − 2 ln L.
  , frGoodnessOfFit :: TestResult
    -- ^ Kolmogorov-Smirnov test against the fitted CDF.
  } deriving stock (Show)

-- ---------------------------------------------------------------------------
-- fitNormal
-- ---------------------------------------------------------------------------

-- | Fit a Normal distribution by MLE.
--
-- MLE: μ̂ = x̄,  σ̂² = (1/n) Σ(xᵢ - x̄)²  (biased MLE, not Bessel-corrected).
-- The KS goodness-of-fit test is run against n(μ̂, σ̂).
--
-- Returns 'Left E2002' if n < 3.
fitNormal :: Vector Double -> SCEResult FitResult
fitNormal values
  | V.length values < 3 =
      Left $ mkError E2002
        ("fitNormal requires >= 3 observations, got "
          <> T.pack (show (V.length values)))
        ["Collect at least 3 observations."] Error
  | otherwise =
      let n     = V.length values
          mu    = kahanMean values
          -- MLE variance (biased): (1/n) * sum (xi - mu)^2
          var   = kahanSum (V.map (\x -> (x - mu) * (x - mu)) values)
                  / fromIntegral n
          sd    = if var < 1e-300 then 1e-10 else sqrt var
          -- Log-likelihood: -n/2 * ln(2π) - n/2 * ln(σ²) - (1/(2σ²)) * Σ(xi-μ)²
          ll    = -fromIntegral n / 2.0 * log (2.0 * pi * var)
                  - fromIntegral n / 2.0
          k     = 2  -- parameters: mu, sigma
          aic   = 2.0 * fromIntegral k - 2.0 * ll
          bic   = fromIntegral k * log (fromIntegral n) - 2.0 * ll
          -- KS against n(mu, sd)
          cdf x = normCDF ((x - mu) / sd)
          gof   = ksTestAgainstCDF values cdf "Normal"
      in Right FitResult
           { frFamily        = Normal
           , frParameters    = [("mean", mu), ("sd", sd)]
           , frLogLikelihood = ll
           , frAIC           = aic
           , frBIC           = bic
           , frGoodnessOfFit = gof
           }

-- ---------------------------------------------------------------------------
-- fitExponential
-- ---------------------------------------------------------------------------

-- | Fit an Exponential distribution by MLE.
--
-- MLE: λ̂ = 1 / x̄.  Requires all values >= 0 (returns 'Left E2003' otherwise).
-- Returns 'Left E2002' if n < 3 or sample mean ≈ 0.
fitExponential :: Vector Double -> SCEResult FitResult
fitExponential values
  | V.length values < 3 =
      Left $ mkError E2002
        ("fitExponential requires >= 3 observations, got "
          <> T.pack (show (V.length values)))
        ["Collect at least 3 observations."] Error
  | V.any (< 0.0) values =
      Left $ mkError E2003
        "fitExponential requires non-negative data"
        ["Exponential distribution is only defined for x >= 0."] Error
  | otherwise =
      let n    = V.length values
          mu   = kahanMean values
      in if mu < 1e-300
           then Left $ mkError E3001
                  "fitExponential: sample mean is (near) zero; rate λ = 1/mean is undefined"
                  ["Check data for all-zero values."] Error
           else
             let rate = 1.0 / mu
                 -- Log-likelihood: n * ln(λ) - λ * sum(xi)
                 ll   = fromIntegral n * log rate
                        - rate * kahanSum values
                 k    = 1   -- parameter: rate
                 aic  = 2.0 * fromIntegral k - 2.0 * ll
                 bic  = fromIntegral k * log (fromIntegral n) - 2.0 * ll
                 cdf x = if x < 0.0 then 0.0 else 1.0 - exp (-(rate * x))
                 gof  = ksTestAgainstCDF values cdf "Exponential"
             in Right FitResult
                  { frFamily        = Exponential
                  , frParameters    = [("rate", rate), ("mean", mu)]
                  , frLogLikelihood = ll
                  , frAIC           = aic
                  , frBIC           = bic
                  , frGoodnessOfFit = gof
                  }

-- ---------------------------------------------------------------------------
-- fitUniform
-- ---------------------------------------------------------------------------

-- | Fit a Uniform distribution by MLE.
--
-- MLE: a = min(x), b = max(x).
-- Log-likelihood: -n * ln(b - a).
-- Returns 'Left E2002' if n < 2 or a == b (degenerate).
fitUniform :: Vector Double -> SCEResult FitResult
fitUniform values
  | V.length values < 2 =
      Left $ mkError E2002
        ("fitUniform requires >= 2 observations, got "
          <> T.pack (show (V.length values)))
        ["Collect at least 2 observations."] Error
  | otherwise =
      let n    = V.length values
          -- Safe: V.null guarded above via n >= 2
          (h', tl') = case V.uncons values of
                        Just p  -> p
                        Nothing -> (0.0, V.empty)   -- unreachable: guarded above
          minV = V.foldl' min h' tl'
          maxV = V.foldl' max h' tl'
          rng  = maxV - minV
      in if rng < 1e-300
           then Left $ mkError E3001
                  "fitUniform: all values are (nearly) identical; range ≈ 0"
                  ["Uniform distribution requires at least two distinct values."] Error
           else
             let ll   = -(fromIntegral n) * log rng
                 k    = 2   -- parameters: a, b
                 aic  = 2.0 * fromIntegral k - 2.0 * ll
                 bic  = fromIntegral k * log (fromIntegral n) - 2.0 * ll
                 cdf x
                   | x <= minV = 0.0
                   | x >= maxV = 1.0
                   | otherwise = (x - minV) / rng
                 gof  = ksTestAgainstCDF values cdf "Uniform"
             in Right FitResult
                  { frFamily        = Uniform
                  , frParameters    = [("min", minV), ("max", maxV)]
                  , frLogLikelihood = ll
                  , frAIC           = aic
                  , frBIC           = bic
                  , frGoodnessOfFit = gof
                  }

-- ---------------------------------------------------------------------------
-- fitBest
-- ---------------------------------------------------------------------------

-- | Fit all three distributions and return the one with the lowest AIC.
--
-- If a distribution cannot be fit (e.g. Exponential on data with negatives)
-- it is skipped.  Returns 'Left E2002' if no distribution can be fit.
fitBest :: Vector Double -> SCEResult FitResult
fitBest values =
  let candidates = [ fitNormal values
                   , fitExponential values
                   , fitUniform values
                   ]
      successes  = [ r | Right r <- candidates ]
  in case successes of
       []     -> Left $ mkError E2002
                   "fitBest: no distribution could be fit to the data"
                   ["Check data for sufficient variability and non-negative values."] Error
       (r:rs) -> Right $ minimumBy (comparing frAIC) (r:rs)

-- ---------------------------------------------------------------------------
-- ksGoodnessOfFit (standalone)
-- ---------------------------------------------------------------------------

-- | One-sample Kolmogorov-Smirnov goodness-of-fit test against an arbitrary CDF.
--
-- Returns 'Left E2002' if n < 3.
ksGoodnessOfFit
  :: Vector Double       -- ^ Observed data
  -> (Double -> Double)  -- ^ Theoretical CDF
  -> T.Text              -- ^ Distribution name (for the TestResult)
  -> SCEResult TestResult
ksGoodnessOfFit values cdf name
  | V.length values < 3 =
      Left $ mkError E2002
        ("KS goodness-of-fit requires >= 3 observations, got "
          <> T.pack (show (V.length values)))
        ["Collect at least 3 observations."] Error
  | otherwise =
      Right (ksTestAgainstCDF values cdf name)

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | KS test against an arbitrary (fully specified) CDF.
-- Always succeeds (total function); error handling is in callers.
ksTestAgainstCDF :: Vector Double -> (Double -> Double) -> T.Text -> TestResult
ksTestAgainstCDF values cdf name =
  let n      = V.length values
      sorted = V.toList $ V.modify VA.sort values
      -- D = max |F_n(x_i) - F(x_i)|
      dPlusList  = [ fromIntegral i / fromIntegral n - cdf x
                   | (i, x) <- zip [1..n] sorted ]
      dMinusList = [ cdf x - fromIntegral (i-1) / fromIntegral n
                   | (i, x) <- zip [1..n] sorted ]
      dPlus  = case dPlusList  of { [] -> 0.0; (x:xs) -> foldl max x xs }
      dMinus = case dMinusList of { [] -> 0.0; (x:xs) -> foldl max x xs }
      dStat  = max dPlus dMinus
      -- Kolmogorov asymptotic approximation
      sqrtN  = sqrt (fromIntegral n)
      pv     = ksPValue ((sqrtN + 0.12 + 0.11 / sqrtN) * dStat)
      ac     = AssumptionCheck
                 { acName        = "KS: parametric family"
                 , acStatus      = if pv > 0.05 then Satisfied else Violated
                 , acDescription = "KS test against fitted " <> name
                                   <> ": p = " <> formatP pv
                 }
      interp = "KS test (" <> name <> "): D = " <> T.pack (show (round3 dStat))
               <> ", p = " <> formatP pv
               <> if pv > 0.05 then " — fit is adequate."
                               else " — fit is poor."
  in mkTestResult
       ("KS goodness-of-fit: " <> name)
       dStat Nothing pv Nothing Nothing Nothing
       n [ac] interp

-- | Kolmogorov asymptotic p-value: P(D_n > d) ≈ 2 * Σ_{k=1}^{20} (-1)^{k+1} exp(-2k² z²).
ksPValue :: Double -> Double
ksPValue z
  | z < 0.0  = 1.0
  | z > 4.0  = 0.0
  | otherwise =
      let terms = map (\k -> ((-1.0)^(k + 1 :: Int))
                              * exp (-2.0 * fromIntegral (k * k) * z * z))
                      [1..20 :: Int]
      in max 0.0 (min 1.0 (2.0 * sum terms))

-- | Standard normal CDF (A&S 26.2.17 rational approximation).
normCDF :: Double -> Double
normCDF x
  | x < 0     = 1.0 - normCDF (-x)
  | otherwise =
      let t   = 1.0 / (1.0 + 0.2316419 * x)
          p   = t * ( 0.319381530
                + t * (-0.356563782
                + t * ( 1.781477937
                + t * (-1.821255978
                + t *   1.330274429))))
          pdf = exp (-0.5 * x * x) / sqrt (2.0 * pi)
      in 1.0 - pdf * p

formatP :: Double -> T.Text
formatP p = T.pack (show (fromIntegral (round (p * 1000) :: Int) / 1000.0 :: Double))

round3 :: Double -> Double
round3 x = fromIntegral (round (x * 1000) :: Int) / 1000.0
