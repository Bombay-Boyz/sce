{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData         #-}

{-|
Module      : SCE.Statistics.Assumptions
Description : Normality and variance-homogeneity tests (Phase 3)
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Assumption tests are prerequisites for choosing inferential tests.  They
are called automatically by the 'SCE.Statistics.Inference' module and
embedded as 'AssumptionCheck' values in every 'TestResult'.

== Tests implemented

  * Shapiro-Wilk (1965) — recommended for n in [3, 50].
    Approximation via Royston (1992) weights.
  * Kolmogorov-Smirnov (one-sample, against standard normal after standardisation).
  * Anderson-Darling (Stephens 1974 critical values).
  * Levene's test for homogeneity of variances (Brown-Forsythe variant:
    uses medians, more robust than original means-based version).
  * Bartlett's test for homogeneity of variances (assumes normality within groups).

== Design constraints
  * No partial functions.
  * All public functions return SCEResult.
  * P-values are clamped to [0, 1].
-}
module SCE.Statistics.Assumptions
  ( -- * Normality tests
    shapiroWilk
  , kolmogorovSmirnov
  , andersonDarling
    -- * Variance homogeneity
  , levenesTest
  , bartlettTest
    -- * Types
  , NormalityResult(..)
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
import           Data.List   (sort)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Result of a normality test.
data NormalityResult = NormalityResult
  { nrTestName   :: T.Text
  , nrStatistic  :: Double   -- ^ Test statistic
  , nrPValue     :: Double   -- ^ p-value in [0, 1]
  , nrIsNormal   :: Bool     -- ^ @p > 0.05@
  , nrSampleSize :: Int
  , nrWarning    :: Maybe T.Text  -- ^ e.g. low-power warning for n < 8
  } deriving stock (Show, Eq)

mkNormality :: T.Text -> Double -> Double -> Int -> NormalityResult
mkNormality name stat pv n = NormalityResult
  { nrTestName   = name
  , nrStatistic  = stat
  , nrPValue     = max 0.0 (min 1.0 pv)
  , nrIsNormal   = pv > 0.05
  , nrSampleSize = n
  , nrWarning    = if n < 8
                     then Just "n < 8: normality test has low statistical power"
                     else Nothing
  }

-- ---------------------------------------------------------------------------
-- Shapiro-Wilk
-- ---------------------------------------------------------------------------

-- | Shapiro-Wilk normality test.
--
-- Recommended for n in [3, 50].  Uses Royston's (1992) approximation for
-- the weights and p-value transformation.  For n > 50 the test is still
-- computed but loses precision; KS or Anderson-Darling is preferred then.
--
-- Returns 'Left E4002' if n < 3.
shapiroWilk :: Vector Double -> SCEResult NormalityResult
shapiroWilk values
  | V.length values < 3 =
      Left $ mkError E4002
        ("Shapiro-Wilk requires >= 3 observations, got "
          <> T.pack (show (V.length values)))
        ["Collect at least 3 observations."] Error
  | otherwise =
      let sorted = V.toList $ V.modify VA.sort values
          n      = length sorted
          w      = swStatistic sorted
          -- Royston's log-transformation of W to approximately normal.
          -- log(1 - W) is clamped away from -Inf to keep z well-defined.
          lnW    = log (max 1e-300 (1.0 - w))
          mu     = swMu n
          sigma  = swSigma n
          z      = (lnW - mu) / sigma
          pv     = 2.0 * normSFUpper (abs z)
      in Right (mkNormality "Shapiro-Wilk" w pv n)

-- | Shapiro-Wilk W statistic.
-- Uses safe vector indexing via 'V.!?' with a 0 fallback (unreachable given
-- the guarded half/n relationship, but avoids partial '!!').
swStatistic :: [Double] -> Double
swStatistic xs =
  let n    = length xs
      xArr = V.fromList xs
      xVec = xArr   -- same vector, for safe indexing below
      mn   = kahanMean xArr
      ws   = swWeights n
      half = n `div` 2
      -- a_i * (x_{n-i} - x_i) pairs; use safe V.!? with 0 default
      num  = sum [ w * (safeIdx xVec (n - 1 - i) - safeIdx xVec i)
                 | (i, w) <- zip [0..half-1] ws ]
      num2 = num * num
      den  = kahanSum $ V.map (\x -> (x - mn) * (x - mn)) xArr
  in if den < 1e-300 then 1.0 else min 1.0 (num2 / den)
  where
    safeIdx v i = case v V.!? i of { Just x -> x; Nothing -> 0.0 }

-- | Approximate Shapiro-Wilk weights using Royston (1992) polynomial.
-- These are not exact for all n but sufficient for the approximation.
swWeights :: Int -> [Double]
swWeights n =
  let half  = n `div` 2
      ms     = [ normalQuantile ((fromIntegral i - 0.375) / (fromIntegral n + 0.25))
               | i <- [1..half] ]
      m2sum  = sum (map (\m -> m * m) ms)
      msqrt  = sqrt m2sum
  in map (/ msqrt) ms

-- | Approximation to the mean of log(1 - W) for Shapiro-Wilk.
--
-- FIX: original formula  @-log (0.459*n - 2.273)@ produces log of a negative
-- number for n = 3 (gamma = -0.896) and n = 4 (gamma = -0.437), yielding NaN.
-- For n <= 4 we use the n = 5 fallback clamped; for n in [5, 11] the formula
-- is valid (gamma > 0).  All values are finite.
swMu :: Int -> Double
swMu n
  | n <= 11   =
      -- Clamp to n >= 5 so gamma = 0.459*n - 2.273 is positive.
      let n'    = fromIntegral (max 5 n) :: Double
          gamma = 0.459 * n' - 2.273
      in -log gamma
  | otherwise =
      let nu = log (fromIntegral n)
      in 0.0038915 * nu^(3::Int) - 0.083751 * nu^(2::Int) - 0.31082 * nu - 1.5861

-- | Approximation to the std dev of log(1 - W) for Shapiro-Wilk.
swSigma :: Int -> Double
swSigma n
  | n <= 11   = exp (0.1349 * fromIntegral n - 1.6275)
  | otherwise =
      let nu = log (fromIntegral n)
      in exp (0.0030302 * nu^(2::Int) - 0.082676 * nu - 0.4803)

-- ---------------------------------------------------------------------------
-- Kolmogorov-Smirnov (one-sample against n(mean, sd))
-- ---------------------------------------------------------------------------

-- | One-sample Kolmogorov-Smirnov test.
--
-- Standardises the data to zero mean and unit variance, then tests
-- against the standard normal CDF.  P-value uses the Kolmogorov
-- asymptotic approximation.
--
-- Returns 'Left E4002' if n < 3.
kolmogorovSmirnov :: Vector Double -> SCEResult NormalityResult
kolmogorovSmirnov values
  | V.length values < 3 =
      Left $ mkError E4002
        ("KS test requires >= 3 observations, got "
          <> T.pack (show (V.length values)))
        ["Collect at least 3 observations."] Error
  | otherwise =
      let n      = V.length values
          mn     = kahanMean values
          var    = welfordVariance values
          sd     = if var < 1e-300 then 1.0 else sqrt var
          sorted = sort $ V.toList $ V.map (\x -> (x - mn) / sd) values
          -- D = max |F_n(x) - Phi(x)|
          dPlusList  = [ fromIntegral i / fromIntegral n - normCDF x
                          | (i, x) <- zip [1..n] sorted ]
          dMinusList = [ normCDF x - fromIntegral (i-1) / fromIntegral n
                        | (i, x) <- zip [1..n] sorted ]
          dPlus  = case dPlusList  of { [] -> 0.0; (x:xs) -> foldl max x xs }
          dMinus = case dMinusList of { [] -> 0.0; (x:xs) -> foldl max x xs }
          dStat  = max dPlus dMinus
          -- Kolmogorov's asymptotic p-value
          sqrtN  = sqrt (fromIntegral n)
          pv     = ksPValue ((sqrtN + 0.12 + 0.11 / sqrtN) * dStat)
      in Right (mkNormality "Kolmogorov-Smirnov" dStat pv n)

-- | Kolmogorov asymptotic p-value: P(D_n > d) ≈ 2 * sum_{k=1}^{inf} (-1)^{k+1} * exp(-2k^2 * z^2)
-- Truncated at 20 terms for numerical stability.
ksPValue :: Double -> Double
ksPValue z
  | z < 0.0  = 1.0
  | z > 4.0  = 0.0
  | otherwise =
      let terms = map (\k -> ((-1.0)^(k+1 :: Int)) * exp (-2.0 * fromIntegral (k*k) * z * z))
                      [1..20 :: Int]
      in max 0.0 (min 1.0 (2.0 * sum terms))

-- | Standard normal CDF via rational approximation (A&S 26.2.17).
normCDF :: Double -> Double
normCDF x
  | x < 0     = 1.0 - normCDF (-x)
  | otherwise  =
      let t = 1.0 / (1.0 + 0.2316419 * x)
          p = t * (0.319381530
              + t * (-0.356563782
              + t * ( 1.781477937
              + t * (-1.821255978
              + t *   1.330274429))))
          pdf = exp (-0.5 * x * x) / sqrt (2.0 * pi)
      in 1.0 - pdf * p

-- ---------------------------------------------------------------------------
-- Anderson-Darling
-- ---------------------------------------------------------------------------

-- | Anderson-Darling normality test.
--
-- Standardises data to n(0,1), computes the A^2 statistic, applies
-- Stephens's (1974) finite-sample correction, then looks up the p-value
-- via an approximation due to Marsaglia & Marsaglia (2004).
--
-- Returns 'Left E4002' if n < 3.
andersonDarling :: Vector Double -> SCEResult NormalityResult
andersonDarling values
  | V.length values < 3 =
      Left $ mkError E4002
        ("Anderson-Darling requires >= 3 observations, got "
          <> T.pack (show (V.length values)))
        ["Collect at least 3 observations."] Error
  | otherwise =
      let n      = V.length values
          mn     = kahanMean values
          var    = welfordVariance values
          sd     = if var < 1e-300 then 1.0 else sqrt var
          -- sorted CDF values; guaranteed in (0,1) because normCDF is strictly
          -- between 0 and 1 for all finite inputs, but we clamp anyway.
          sorted = V.fromList $ sort
                              $ map (\x -> max 1e-300 (min (1.0 - 1e-300)
                                             (normCDF ((x - mn) / sd))))
                              $ V.toList values
          n'     = fromIntegral n :: Double
          -- A^2 = -n - (1/n) * sum_{i=1}^{n} (2i-1) * [ln u_i + ln(1 - u_{n+1-i})]
          -- Safe indexing via V.!? with a neutral fallback (unreachable given
          -- the clamped sorted vector of length n).
          aRaw   = negate n' - (1.0 / n') *
                   sum [ fromIntegral (2*i - 1)
                         * (  safeLog (sorted V.!? (i - 1))
                           +  safeLog1m (sorted V.!? (n - i)))
                       | i <- [1..n] ]
          -- Stephens correction for composite normality test
          aStar  = aRaw * (1.0 + 0.75 / n' + 2.25 / (n' * n'))
          pv     = adPValue aStar
      in Right (mkNormality "Anderson-Darling" aRaw pv n)
  where
    -- log u, with a safe floor to avoid -Inf when u is very close to 0.
    safeLog  mx = case mx of { Just u -> log (max 1e-300 u);             Nothing -> 0.0 }
    -- log(1 - u), with a safe floor to avoid -Inf when u is very close to 1.
    safeLog1m mx = case mx of { Just u -> log (max 1e-300 (1.0 - u)); Nothing -> 0.0 }

-- | AD p-value approximation (Marsaglia & Marsaglia 2004).
adPValue :: Double -> Double
adPValue a
  | a < 0.0   = 1.0
  | a < 0.2   = 1.0 - exp (-13.436 + 101.14 * a - 223.73 * a^(2::Int))
  | a < 0.34  = 1.0 - exp (-8.318  + 42.796 * a - 59.938 * a^(2::Int))
  | a < 0.6   = exp (0.9177 - 4.279 * a - 1.38  * a^(2::Int))
  | a < 13.0  = exp (1.2937 - 5.709 * a + 0.0186 * a^(2::Int))
  | otherwise = 0.0

-- ---------------------------------------------------------------------------
-- Levene's test (Brown-Forsythe variant)
-- ---------------------------------------------------------------------------

-- | Levene's test for homogeneity of variances across groups.
--
-- Uses the Brown-Forsythe variant (group medians instead of means),
-- which is more robust to non-normality.
--
-- Requires at least 2 groups, each with >= 2 observations.
-- Returns 'Left E4002' if any group has n < 2.
levenesTest :: [Vector Double] -> SCEResult TestResult
levenesTest groups
  | length groups < 2 =
      Left $ mkError E4002
        "Levene's test requires at least 2 groups"
        ["Provide data for at least 2 groups."] Error
  | any (\g -> V.length g < 2) groups =
      Left $ mkError E4002
        "Every group must have >= 2 observations for Levene's test"
        ["Collect more data in each group."] Error
  | otherwise =
      let k   = length groups
          ns  = map V.length groups
          n   = sum ns
          -- Brown-Forsythe: z_ij = |x_ij - median_j|
          zss = map (\g ->
                  let med = groupMedian g
                  in V.map (\x -> abs (x - med)) g
                ) groups
          -- Overall mean of all z values
          zbar = kahanMean (V.concat zss)
          -- Group means of z
          zbars = map kahanMean zss
          -- Between-group SS
          ssBG = sum [ fromIntegral ni * (zb - zbar)^(2::Int)
                     | (ni, zb) <- zip ns zbars ]
          -- Within-group SS
          ssWG = sum [ kahanSum (V.map (\z -> (z - zb)^(2::Int)) zs)
                     | (zs, zb) <- zip zss zbars ]
          dfBG = fromIntegral (k - 1) :: Double
          dfWG = fromIntegral (n - k) :: Double
          msBG = ssBG / dfBG
          msWG = if ssWG < 1e-300 then 1e-300 else ssWG / dfWG
          fStat = msBG / msWG
          pv   = fPValue fStat dfBG dfWG
          sig  = pv < 0.05
          ac   = AssumptionCheck
                   { acName        = "Equal variances (Levene)"
                   , acStatus      = if sig then Violated else Satisfied
                   , acDescription = if sig
                       then "Levene's test: variances differ significantly (p = "
                              <> formatP pv <> "); use Welch's t-test."
                       else "Levene's test: variances are homogeneous (p = "
                              <> formatP pv <> ")."
                   }
          interp = if sig
                     then "Significant variance heterogeneity detected (F = "
                            <> T.pack (show (round2 fStat)) <> ", p = " <> formatP pv <> ")"
                     else "No significant variance heterogeneity detected (p = "
                            <> formatP pv <> ")"
      in Right $ mkTestResult
           "Levene's test (Brown-Forsythe)"
           fStat
           (Just dfBG)
           pv
           Nothing
           Nothing
           Nothing
           n
           [ac]
           interp

-- | Median of a vector.  Precondition: V.length v >= 1.
-- Uses safe V.!? indexing with a 0.0 fallback (unreachable given the guard).
groupMedian :: Vector Double -> Double
groupMedian v
  | V.null v  = 0.0
  | otherwise =
      let sorted = V.modify VA.sort v
          len    = V.length sorted
          mid    = len `div` 2
          safeAt i = case sorted V.!? i of { Just x -> x; Nothing -> 0.0 }
      in if odd len
           then safeAt mid
           else (safeAt (mid - 1) + safeAt mid) / 2.0

-- ---------------------------------------------------------------------------
-- Bartlett's test
-- ---------------------------------------------------------------------------

-- | Bartlett's test for homogeneity of variances.
--
-- Assumes normality within groups; more powerful than Levene's when
-- normality holds, but sensitive to departures from it.
--
-- Requires at least 2 groups, each with >= 2 observations.
-- Returns 'Left E4002' if any group has n < 2.
bartlettTest :: [Vector Double] -> SCEResult TestResult
bartlettTest groups
  | length groups < 2 =
      Left $ mkError E4002
        "Bartlett's test requires at least 2 groups"
        ["Provide data for at least 2 groups."] Error
  | any (\g -> V.length g < 2) groups =
      Left $ mkError E4002
        "Every group must have >= 2 observations for Bartlett's test"
        ["Collect more data in each group."] Error
  | otherwise =
      let k     = length groups
          ns    = map V.length groups
          n     = sum ns
          dfs   = map (\ni -> fromIntegral ni - 1.0) ns :: [Double]
          dfTot = fromIntegral n - fromIntegral k :: Double
          vars  = map welfordVariance groups
          -- Pooled variance
          spSq  = sum (zipWith (*) dfs vars) / dfTot
          -- Bartlett's statistic
          lnSp  = if spSq < 1e-300 then -300.0 else log spSq
          num   = dfTot * lnSp - sum (zipWith (\df v ->
                    df * if v < 1e-300 then -300.0 else log v) dfs vars)
          -- Correction factor
          cFact = 1.0 + (1.0 / (3.0 * fromIntegral (k - 1)))
                      * (sum (map (1.0 /) dfs) - 1.0 / dfTot)
          chiSq = num / cFact
          dfChi = fromIntegral (k - 1) :: Double
          pv    = chiSqPValue chiSq dfChi
          sig   = pv < 0.05
          ac    = AssumptionCheck
                    { acName        = "Equal variances (Bartlett)"
                    , acStatus      = if sig then Violated else Satisfied
                    , acDescription = if sig
                        then "Bartlett: variances differ significantly (p = "
                               <> formatP pv <> ")."
                        else "Bartlett: variances are homogeneous (p = "
                               <> formatP pv <> ")."
                    }
          interp = if sig
                     then "Significant variance heterogeneity (chi2 = "
                            <> T.pack (show (round2 chiSq)) <> ", p = " <> formatP pv <> ")"
                     else "No significant variance heterogeneity (p = "
                            <> formatP pv <> ")"
      in Right $ mkTestResult
           "Bartlett's test"
           chiSq
           (Just dfChi)
           pv
           Nothing
           Nothing
           Nothing
           n
           [ac]
           interp

-- ---------------------------------------------------------------------------
-- Distribution quantile helpers
-- ---------------------------------------------------------------------------

-- | Upper tail of the standard normal CDF.
normSFUpper :: Double -> Double
normSFUpper z
  | z < 0     = 1.0 - normSFUpper (-z)
  | otherwise =
      let t   = 1.0 / (1.0 + 0.2316419 * z)
          p   = t * (0.319381530
                + t * (-0.356563782
                + t * ( 1.781477937
                + t * (-1.821255978
                + t *   1.330274429))))
          pdf = exp (-0.5 * z * z) / sqrt (2.0 * pi)
      in pdf * p

-- | Approximate p-value for F(dfN, dfD) distribution.
--
-- Uses Patnaik's two-moment cube-root normal approximation.
-- Adequate for dfD > 10; for small dfD the error grows but remains
-- usable for the exploratory purpose of assumption testing.
--
-- FIX (consistency): formula now matches the one in 'SCE.Statistics.Inference'
-- exactly. The original Assumptions.hs had the two variance terms swapped in
-- the denominator, producing different p-values for the same F statistic.
-- Canonical formula: z = (x*A - B) / sqrt(A/x^2 + C)
-- where x = (f/dfN)^{1/3}, A = 1-2/(9*dfN), B = 1-2/(9*dfD),
--           C = 2/(9*dfD), and the A/x^2 term = 2/(9*dfN) / (f/dfN)^{2/3}.
fPValue :: Double -> Double -> Double -> Double
fPValue f dfN dfD
  | f <= 0.0  = 1.0
  | dfN <= 0.0 || dfD <= 0.0 = 1.0
  | otherwise =
      let z = ((f / dfN) ** (1.0/3.0) * (1.0 - 2.0/(9.0*dfN))
               - (1.0 - 2.0/(9.0*dfD)))
              / sqrt (2.0/(9.0*dfN) / ((f/dfN) ** (2.0/3.0))
                     + 2.0/(9.0*dfD))
      in max 0.0 (min 1.0 (normSFUpper z))

-- | Chi-square p-value: P(X^2_{df} > chi2).
-- Uses Wilson-Hilferty cube-root normal approximation.
chiSqPValue :: Double -> Double -> Double
chiSqPValue chi2 df
  | chi2 <= 0.0 = 1.0
  | df <= 0.0   = 0.0
  | otherwise   =
      let z = ((chi2 / df)** (1.0/3.0) - (1.0 - 2.0 / (9.0 * df)))
              / sqrt (2.0 / (9.0 * df))
      in max 0.0 (min 1.0 (normSFUpper z))

-- ---------------------------------------------------------------------------
-- Normal quantile (Acklam)
-- ---------------------------------------------------------------------------

normalQuantile :: Double -> Double
normalQuantile p
  | p <= 0.0  = -1.0e15
  | p >= 1.0  =  1.0e15
  | p < 0.5   = -(rationalApprox (sqrt (-2.0 * log p)))
  | otherwise =   rationalApprox (sqrt (-2.0 * log (1.0 - p)))
  where
    rationalApprox t =
      let num = t * ( -3.969683028665376e+01
              + t * (  2.209460984245205e+02
              + t * ( -2.759285104469687e+02
              + t * (  1.383577518672690e+02
              + t * ( -3.066479806614716e+01
              + t *    2.506628277459239e+00)))))
          den = 1.0
              + t * ( -5.447609879822406e+01
              + t * (  1.615858368580409e+02
              + t * ( -1.556989798598866e+02
              + t * (  6.680131188771972e+01
              + t *  (-1.328068155288572e+01)))))
      in num / den

-- ---------------------------------------------------------------------------
-- Formatting helpers
-- ---------------------------------------------------------------------------

formatP :: Double -> T.Text
formatP p = T.pack (show (fromIntegral (round (p * 1000) :: Int) / 1000.0 :: Double))

round2 :: Double -> Double
round2 x = fromIntegral (round (x * 100) :: Int) / 100.0
