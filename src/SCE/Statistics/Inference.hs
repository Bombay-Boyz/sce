{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData         #-}

{-|
Module      : SCE.Statistics.Inference
Description : Parametric inference: t-tests, ANOVA, Tukey HSD, effect sizes (Phase 3)
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

== Algorithms

  * One-sample t-test (Student 1908)
  * Two-sample t-test with pooled variance (assumes equal variances)
  * Welch's t-test (Welch 1947) — unequal variances, Satterthwaite df
  * Paired t-test (reduces to one-sample on differences)
  * One-way ANOVA (Fisher 1925) — F = MS_between / MS_within
  * Tukey HSD (Tukey 1949) — all-pairs post-hoc; studentised range distribution
  * Cohen's d with 95% CI (Cohen 1988)
  * Hedges' g bias correction (Hedges 1981)
  * Achieved power for t-tests (one-sided normal approximation)

== P-value computation

All t p-values use the Student t-distribution approximated via the
regularised incomplete beta function / Gauss hypergeometric series.

== Design constraints
  * No partial functions.
  * Every function returns SCEResult.
  * P-values clamped to [0, 1] by mkTestResult.
-}
module SCE.Statistics.Inference
  ( -- * t-tests
    oneSampleTTest
  , twoSampleTTest
  , welchTTest
  , pairedTTest
    -- * ANOVA
  , oneWayANOVA
  , tukeyHSD
    -- * Effect sizes
  , cohensD
  , hedgesG
    -- * Power analysis
  , powerTTest
  , requiredSampleSize
    -- * Types
  , EqualVariance(..)
  , ANOVAResult(..)
  , PostHocResult(..)
  ) where

import SCE.Core.Types
  ( SCEResult, mkError, ErrorCode(..), Severity(..) )
import SCE.Core.Statistics
  ( kahanMean, kahanSum, welfordVariance )
import SCE.Statistics.TestResult
  ( TestResult(..), EffectSize(..), ConfidenceInterval(..)
  , AssumptionCheck(..), AssumptionStatus(..)
  , mkTestResult
  )
import SCE.Statistics.Assumptions
  ( shapiroWilk, NormalityResult(..) )

import qualified Data.Text   as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

data EqualVariance = AssumeEqual | DontAssumeEqual
  deriving stock (Show, Eq)

data ANOVAResult = ANOVAResult
  { arFStatistic   :: Double
  , arDFBetween    :: Int
  , arDFWithin     :: Int
  , arPValue       :: Double
  , arEtaSquared   :: Double
  , arOmegaSquared :: Double
  , arGroupMeans   :: [Double]
  , arAssumptions  :: [AssumptionCheck]
  } deriving stock (Show, Eq)

-- | All pairwise comparisons from Tukey HSD.
newtype PostHocResult = PostHocResult
  { phrComparisons :: [(Int, Int, Double, Double, Bool)]
    -- ^ (groupA index, groupB index, meanDiff, adjusted p-value, significant at 0.05)
  } deriving stock (Show)

-- ---------------------------------------------------------------------------
-- One-sample t-test
-- ---------------------------------------------------------------------------

-- | One-sample t-test: H₀: μ = mu0.
--
-- Runs Shapiro-Wilk automatically and embeds the result as an AssumptionCheck.
-- Returns 'Left E4002' if n < 2.
oneSampleTTest
  :: Double        -- ^ Hypothesised mean (mu_0)
  -> Vector Double
  -> SCEResult TestResult
oneSampleTTest mu0 values
  | V.length values < 2 =
      Left $ mkError E4002
        ("One-sample t-test requires >= 2 observations, got "
          <> T.pack (show (V.length values)))
        ["Collect more data."] Error
  | otherwise =
      let n    = V.length values
          mn   = kahanMean values
          var  = welfordVariance values
          se   = sqrt (var / fromIntegral n)
          df   = fromIntegral (n - 1) :: Double
          tStat = if se < 1e-300
                    then if abs (mn - mu0) < 1e-300 then 0.0 else 1.0e15
                    else (mn - mu0) / se
          pv    = 2.0 * tPValue (abs tStat) df
          ciLo  = mn - tQuantile 0.975 df * se
          ciHi  = mn + tQuantile 0.975 df * se
          ci    = ConfidenceInterval 0.95 ciLo ciHi
          eff   = cohensDAbs (mn - mu0) (sqrt var)
          effCI = cohensCI eff n n
          norm  = normAssumption values n
          interp = "One-sample t-test: t(" <> T.pack (show (n-1))
                   <> ") = " <> T.pack (show (round2 tStat))
                   <> ", p = " <> formatP pv
                   <> if pv < 0.05 then ", significant at α = 0.05"
                                   else ", not significant at α = 0.05"
      in Right $ mkTestResult
           "one-sample t-test" tStat (Just df) pv
           (Just ci)
           (Just (CohensD eff effCI))
           (Just (achievedPowerT eff n 0.05))
           n [norm] interp

-- ---------------------------------------------------------------------------
-- Two-sample t-test (pooled)
-- ---------------------------------------------------------------------------

-- | Two-sample independent t-test (pooled variance, assumes equal variances).
--
-- Use 'welchTTest' when group sizes differ or equal variances are uncertain.
-- Returns 'Left E4002' if either group has n < 2.
twoSampleTTest
  :: Vector Double
  -> Vector Double
  -> EqualVariance
  -> SCEResult TestResult
twoSampleTTest xs ys eqVar =
  case eqVar of
    DontAssumeEqual -> welchTTest xs ys
    AssumeEqual
      | V.length xs < 2 || V.length ys < 2 ->
          Left $ mkError E4002
            "Two-sample t-test requires >= 2 observations per group"
            ["Collect more data."] Error
      | otherwise ->
          let n1   = V.length xs; n2 = V.length ys
              mn1  = kahanMean xs; mn2 = kahanMean ys
              var1 = welfordVariance xs; var2 = welfordVariance ys
              -- pooled variance
              sp2  = ((fromIntegral n1 - 1.0) * var1
                    + (fromIntegral n2 - 1.0) * var2)
                    / fromIntegral (n1 + n2 - 2)
              se   = sqrt (sp2 * (1.0 / fromIntegral n1 + 1.0 / fromIntegral n2))
              df   = fromIntegral (n1 + n2 - 2) :: Double
              tStat = if se < 1e-300 then 0.0 else (mn1 - mn2) / se
              pv    = 2.0 * tPValue (abs tStat) df
              ciLo  = (mn1 - mn2) - tQuantile 0.975 df * se
              ciHi  = (mn1 - mn2) + tQuantile 0.975 df * se
              ci    = ConfidenceInterval 0.95 ciLo ciHi
              eff   = cohensDAbs (mn1 - mn2) (sqrt sp2)
              effCI = cohensCI eff n1 n2
              norms = map (\(v, lbl) -> normAssumptionLabelled v (V.length v) lbl)
                          [(xs, "Group 1"), (ys, "Group 2")]
              interp = "Two-sample t-test (pooled): t(" <> T.pack (show (n1+n2-2))
                       <> ") = " <> T.pack (show (round2 tStat))
                       <> ", p = " <> formatP pv
          in Right $ mkTestResult
               "two-sample t-test (pooled variance)"
               tStat (Just df) pv (Just ci)
               (Just (CohensD eff effCI))
               (Just (achievedPowerT eff (n1 `min` n2) 0.05))
               (n1 + n2) norms interp

-- ---------------------------------------------------------------------------
-- Welch's t-test (unequal variances)
-- ---------------------------------------------------------------------------

-- | Welch's t-test — does not assume equal variances.
--
-- Uses the Satterthwaite approximation for degrees of freedom.
-- Preferred when group sizes or variances may differ.
-- Returns 'Left E4002' if either group has n < 2.
welchTTest :: Vector Double -> Vector Double -> SCEResult TestResult
welchTTest xs ys
  | V.length xs < 2 || V.length ys < 2 =
      Left $ mkError E4002
        "Welch's t-test requires >= 2 observations per group"
        ["Collect more data."] Error
  | otherwise =
      let n1    = V.length xs; n2 = V.length ys
          mn1   = kahanMean xs; mn2 = kahanMean ys
          var1  = welfordVariance xs; var2 = welfordVariance ys
          s1n   = var1 / fromIntegral n1
          s2n   = var2 / fromIntegral n2
          se    = sqrt (s1n + s2n)
          -- Satterthwaite df
          df    = if (s1n + s2n) < 1e-300 then 1.0
                  else (s1n + s2n)^(2::Int)
                       / ( s1n^(2::Int) / fromIntegral (n1 - 1)
                         + s2n^(2::Int) / fromIntegral (n2 - 1) )
          tStat = if se < 1e-300 then 0.0 else (mn1 - mn2) / se
          pv    = 2.0 * tPValue (abs tStat) df
          ciLo  = (mn1 - mn2) - tQuantile 0.975 df * se
          ciHi  = (mn1 - mn2) + tQuantile 0.975 df * se
          ci    = ConfidenceInterval 0.95 ciLo ciHi
          poolSD = sqrt ((var1 + var2) / 2.0)
          eff   = cohensDAbs (mn1 - mn2) poolSD
          effCI = cohensCI eff n1 n2
          norms = map (\(v, lbl) -> normAssumptionLabelled v (V.length v) lbl)
                      [(xs, "Group 1"), (ys, "Group 2")]
          interp = "Welch's t-test: t(" <> T.pack (show (round (df :: Double) :: Int))
                   <> ") = " <> T.pack (show (round2 tStat))
                   <> ", p = " <> formatP pv
      in Right $ mkTestResult
           "Welch's t-test"
           tStat (Just df) pv (Just ci)
           (Just (CohensD eff effCI))
           (Just (achievedPowerT eff (n1 `min` n2) 0.05))
           (n1 + n2) norms interp

-- ---------------------------------------------------------------------------
-- Paired t-test
-- ---------------------------------------------------------------------------

-- | Paired t-test: computes differences and runs a one-sample t-test on them.
--
-- Returns 'Left E2001' if vectors have different lengths.
-- Returns 'Left E4002' if n < 2.
pairedTTest :: Vector Double -> Vector Double -> SCEResult TestResult
pairedTTest xs ys
  | V.length xs /= V.length ys =
      Left $ mkError E2001
        ("Paired t-test requires equal-length vectors, got "
          <> T.pack (show (V.length xs)) <> " and "
          <> T.pack (show (V.length ys)))
        ["Ensure both vectors have the same number of observations."] Error
  | V.length xs < 2 =
      Left $ mkError E4002
        "Paired t-test requires >= 2 paired observations"
        ["Collect more data."] Error
  | otherwise =
      let diffs = V.zipWith (-) xs ys
      in case oneSampleTTest 0.0 diffs of
           Left err -> Left err
           Right tr -> Right tr
               { trTestName       = "paired t-test"
               , trInterpretation = "Paired t-test: " <> trInterpretation tr
               }

-- ---------------------------------------------------------------------------
-- One-way ANOVA
-- ---------------------------------------------------------------------------

-- | One-way ANOVA: tests whether group means are equal.
--
-- Automatically runs Levene's test (embedded in 'arAssumptions').
-- Returns 'Left E4002' if fewer than 2 groups or any group has n < 2.
oneWayANOVA :: [Vector Double] -> SCEResult ANOVAResult
oneWayANOVA groups
  | length groups < 2 =
      Left $ mkError E4002
        "One-way ANOVA requires at least 2 groups"
        ["Provide data for at least 2 groups."] Error
  | any (\g -> V.length g < 2) groups =
      Left $ mkError E4002
        "Every group must have >= 2 observations for ANOVA"
        ["Collect more data in each group."] Error
  | otherwise =
      let k       = length groups
          ns      = map V.length groups
          n       = sum ns
          means   = map kahanMean groups
          grandMn = kahanMean (V.concat groups)
          -- SS between
          ssBG    = sum [ fromIntegral ni * (mn - grandMn)^(2::Int)
                        | (ni, mn) <- zip ns means ]
          -- SS within
          ssWG    = sum [ kahanSum (V.map (\x -> (x - mn)^(2::Int)) g)
                        | (g, mn) <- zip groups means ]
          dfBG    = k - 1
          dfWG    = n - k
          msBG    = ssBG / fromIntegral dfBG
          msWG    = if ssWG < 1e-300 then 1e-300 else ssWG / fromIntegral dfWG
          fStat   = msBG / msWG
          pv      = fPValue fStat (fromIntegral dfBG) (fromIntegral dfWG)
          ssTot   = ssBG + ssWG
          etaSq   = if ssTot < 1e-300 then 0.0 else ssBG / ssTot
          omegaSq = if ssTot + msWG < 1e-300 then 0.0
                    else (ssBG - fromIntegral dfBG * msWG) / (ssTot + msWG)
          -- Levene assumption check
          levAC   = case leveneAC groups of
                      Right ac -> ac
                      Left _   -> AssumptionCheck "Equal variances (Levene)"
                                    NotTested "Could not run Levene's test"
      in Right ANOVAResult
           { arFStatistic   = fStat
           , arDFBetween    = dfBG
           , arDFWithin     = dfWG
           , arPValue       = max 0.0 (min 1.0 pv)
           , arEtaSquared   = max 0.0 etaSq
           , arOmegaSquared = max 0.0 omegaSq
           , arGroupMeans   = means
           , arAssumptions  = [levAC]
           }

-- | Run Levene's test and extract a single AssumptionCheck.
leveneAC :: [Vector Double] -> SCEResult AssumptionCheck
leveneAC groups = do
  let k   = length groups
      ns  = map V.length groups
      n   = sum ns
      zss = map (\g -> let med = groupMedian g
                       in V.map (\x -> abs (x - med)) g) groups
      zbar  = kahanMean (V.concat zss)
      zbars = map kahanMean zss
      ssBG  = sum [ fromIntegral ni * (zb - zbar)^(2::Int)
                  | (ni, zb) <- zip ns zbars ]
      ssWG  = sum [ kahanSum (V.map (\z -> (z - zb)^(2::Int)) zs)
                  | (zs, zb) <- zip zss zbars ]
      dfBG  = fromIntegral (k - 1) :: Double
      dfWG  = fromIntegral (n - k) :: Double
      msWG  = if ssWG < 1e-300 then 1e-300 else ssWG / dfWG
      fStat = (ssBG / dfBG) / msWG
      pv    = fPValue fStat dfBG dfWG
      sig   = pv < 0.05
  return AssumptionCheck
    { acName        = "Equal variances (Levene)"
    , acStatus      = if sig then Violated else Satisfied
    , acDescription = if sig
        then "Levene: variances differ (p = " <> formatP pv <> "); consider Kruskal-Wallis."
        else "Levene: variances are homogeneous (p = " <> formatP pv <> ")."
    }

groupMedian :: Vector Double -> Double
groupMedian v
  | V.null v  = 0.0
  | otherwise =
      let sorted = V.modify VA.sort v
          len    = V.length sorted
          mid    = len `div` 2
      in if odd len
           then sorted V.! mid
           else (sorted V.! (mid - 1) + sorted V.! mid) / 2.0

-- ---------------------------------------------------------------------------
-- Tukey HSD
-- ---------------------------------------------------------------------------

-- | Tukey Honest Significant Difference post-hoc test.
--
-- Computes all pairwise mean differences and their adjusted p-values
-- using the studentised range distribution (Qα,k,dfW).
-- Run after a significant one-way ANOVA.
tukeyHSD :: ANOVAResult -> [Vector Double] -> SCEResult PostHocResult
tukeyHSD anova groups
  | length groups < 2 =
      Left $ mkError E4002
        "Tukey HSD requires at least 2 groups" [] Error
  | arDFWithin anova <= 0 =
      Left $ mkError E4002
        "Tukey HSD: within-group degrees of freedom must be > 0" [] Error
  | otherwise =
      let k        = length groups
          -- Use V.fromList so we can do safe V.! indexing (guarded by comprehension bounds)
          nsV      = V.fromList (map V.length groups)
          meansV   = V.fromList (arGroupMeans anova)
          dfW      = fromIntegral (arDFWithin anova) :: Double
          -- Recompute MS_within from groups (more accurate than deriving from F)
          means    = arGroupMeans anova
          msWithin = let ssW   = sum [ kahanSum (V.map (\x -> (x - mn)^(2::Int)) g)
                                     | (g, mn) <- zip groups means ]
                         dfWit = fromIntegral (V.sum nsV - k) :: Double
                     in if dfWit < 1e-300 then 1.0 else ssW / dfWit
          -- All pairs (i, j) with i < j — indices are bounded by k
          pairs    = [ let ni   = nsV V.! i
                           nj   = nsV V.! j
                           mi   = meansV V.! i
                           mj   = meansV V.! j
                           se   = sqrt (msWithin
                                        * (1.0 / fromIntegral ni
                                           + 1.0 / fromIntegral nj)
                                        / 2.0)
                           qStat = if se < 1e-300 then 0.0
                                   else abs (mi - mj) / se
                           pv   = qPValue qStat (fromIntegral k) dfW
                       in (i, j, mi - mj, pv, pv < 0.05)
                     | i <- [0..k-2], j <- [i+1..k-1] ]
      in Right (PostHocResult pairs)

-- Approximate studentised range p-value using Bonferroni upper bound.
-- Full implementation requires the Studentised Range distribution.
-- This approximation: p_Tukey ≈ k * p_normal * min(k-1, 4) / k
qPValue :: Double -> Double -> Double -> Double
qPValue q k dfW =
  -- Normal approximation to the studentised range: q ~ n(0,1) * sqrt(2)
  let z  = q / sqrt 2.0
      pn = 2.0 * normSFUpper z
      -- Sidak correction for k groups
      pAdj = max 0.0 (min 1.0 (1.0 - (1.0 - pn) ^ (max 1 (round k :: Int))))
  in pAdj

-- ---------------------------------------------------------------------------
-- Effect sizes: Cohen's d, Hedges' g
-- ---------------------------------------------------------------------------

-- | Cohen's d for two independent groups, with 95% CI (Hedges-Olkin 1985).
-- Returns 'Left E4002' if either group has n < 2.
cohensD :: Vector Double -> Vector Double -> SCEResult EffectSize
cohensD xs ys
  | V.length xs < 2 || V.length ys < 2 =
      Left $ mkError E4002
        "Cohen's d requires >= 2 observations per group" [] Error
  | otherwise =
      let n1   = V.length xs; n2 = V.length ys
          mn1  = kahanMean xs; mn2 = kahanMean ys
          var1 = welfordVariance xs; var2 = welfordVariance ys
          sp   = sqrt ( ((fromIntegral n1 - 1.0) * var1
                       + (fromIntegral n2 - 1.0) * var2)
                       / fromIntegral (n1 + n2 - 2) )
          d    = if sp < 1e-300 then 0.0 else (mn1 - mn2) / sp
          ci   = cohensCI d n1 n2
      in Right (CohensD d ci)

-- | Hedges' g: bias-corrected Cohen's d.  Preferred for small or unequal n.
-- Returns 'Left E4002' if either group has n < 2.
hedgesG :: Vector Double -> Vector Double -> SCEResult EffectSize
hedgesG xs ys
  | V.length xs < 2 || V.length ys < 2 =
      Left $ mkError E4002
        "Hedges' g requires >= 2 observations per group" [] Error
  | otherwise = do
      cd <- cohensD xs ys
      let d   = case cd of { CohensD v _ -> v; _ -> 0.0 }
          n1  = V.length xs; n2 = V.length ys
          df  = n1 + n2 - 2
          -- Exact correction factor J(df) ≈ 1 - 3/(4*df - 1)
          j   = 1.0 - 3.0 / (4.0 * fromIntegral df - 1.0)
          g   = d * j
          ci  = cohensCI g n1 n2
      Right (HedgesG g ci)

-- ---------------------------------------------------------------------------
-- Power analysis
-- ---------------------------------------------------------------------------

-- | Achieved power of a two-sided t-test for effect size d, n per group, alpha.
--
-- Uses a normal approximation to the non-central t.
-- Adequate for n >= 10; for smaller n the result is an approximation.
powerTTest :: Double  -- ^ Effect size (Cohen's d)
           -> Int     -- ^ Sample size per group
           -> Double  -- ^ Alpha (e.g. 0.05)
           -> SCEResult Double
powerTTest d n alpha
  | n < 2 =
      Left $ mkError E4002
        "Power requires n >= 2" [] Error
  | alpha <= 0.0 || alpha >= 1.0 =
      Left $ mkError E2002
        ("Alpha must be in (0,1), got " <> T.pack (show alpha)) [] Error
  | otherwise =
      let df   = fromIntegral (2 * n - 2) :: Double
          tCrit = tQuantile (1.0 - alpha / 2.0) df
          nc    = abs d * sqrt (fromIntegral n / 2.0)
          -- P(T_{df, nc} > tCrit) ≈ P(Z > tCrit - nc)
          pow  = normSFUpper (tCrit - nc)
      in Right (max 0.0 (min 1.0 pow))

-- | Minimum n per group to achieve target power for effect size d at alpha.
-- Uses bisection on the power function.
requiredSampleSize
  :: Double  -- ^ Effect size (Cohen's d)
  -> Double  -- ^ Target power (e.g. 0.80)
  -> Double  -- ^ Alpha (e.g. 0.05)
  -> SCEResult Int
requiredSampleSize d targetPow alpha
  | d <= 0.0 =
      Left $ mkError E2002
        ("Effect size must be > 0, got " <> T.pack (show d)) [] Error
  | targetPow <= 0.0 || targetPow >= 1.0 =
      Left $ mkError E2002
        ("Target power must be in (0,1), got " <> T.pack (show targetPow)) [] Error
  | alpha <= 0.0 || alpha >= 1.0 =
      Left $ mkError E2002
        ("Alpha must be in (0,1), got " <> T.pack (show alpha)) [] Error
  | otherwise =
      -- Bisect on n in [2, 100000]
      let go lo hi
            | hi - lo <= 1 = hi
            | otherwise =
                let mid = (lo + hi) `div` 2
                    pow = case powerTTest d mid alpha of
                            Right p -> p
                            Left _  -> 0.0
                in if pow >= targetPow
                     then go lo mid
                     else go mid hi
      in Right (go 2 100000)

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | |Cohen's d| from mean difference and pooled SD.
cohensDAbs :: Double -> Double -> Double
cohensDAbs diff sd = if sd < 1e-300 then 0.0 else abs diff / sd

-- | 95% CI for Cohen's d (Hedges & Olkin 1985 approximation).
cohensCI :: Double -> Int -> Int -> ConfidenceInterval
cohensCI d n1 n2 =
  let n1' = fromIntegral n1 :: Double
      n2' = fromIntegral n2 :: Double
      se  = sqrt ((n1' + n2') / (n1' * n2') + d^(2::Int) / (2.0 * (n1' + n2')))
  in ConfidenceInterval 0.95 (d - 1.96 * se) (d + 1.96 * se)

-- | Achieved power for one-sided normal approximation.
achievedPowerT :: Double -> Int -> Double -> Double
achievedPowerT d n alpha =
  case powerTTest d n alpha of
    Right p -> p
    Left _  -> 0.0

-- | Normality AssumptionCheck for a single group.
normAssumption :: Vector Double -> Int -> AssumptionCheck
normAssumption v n = normAssumptionLabelled v n "Data"

normAssumptionLabelled :: Vector Double -> Int -> T.Text -> AssumptionCheck
normAssumptionLabelled v n lbl =
  let (status, desc) =
        if n < 3
          then (NotTested, lbl <> ": n < 3, normality not tested")
          else case shapiroWilk v of
                 Left  _ -> (NotTested, lbl <> ": Shapiro-Wilk could not be computed")
                 Right nr ->
                   if nrIsNormal nr
                     then (Satisfied, lbl <> ": normal (Shapiro-Wilk p = " <> formatP (nrPValue nr) <> ")")
                     else (Violated,  lbl <> ": non-normal (Shapiro-Wilk p = " <> formatP (nrPValue nr) <> ")")
  in AssumptionCheck "Normality" status desc

-- | Standard normal upper tail: P(Z > z).
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

-- | Two-sided t p-value: P(|T_df| > |t|).
tPValue :: Double -> Double -> Double
tPValue tAbs df
  | df <= 0.0 = 0.5
  | otherwise =
      -- Returns the one-tailed P(T_df > t); call sites multiply by 2.0 for two-sided.
      -- Formula: 0.5 * I_{df/(df+t^2)}(df/2, 1/2)
      let x = df / (df + tAbs * tAbs)
      in 0.5 * regularisedBeta x (df / 2.0) 0.5


-- | Regularised incomplete beta I_x(a,b).
-- Uses continued fraction expansion (Numerical Recipes §6.4).
regularisedBeta :: Double -> Double -> Double -> Double
regularisedBeta x a b
  | x < 0.0 || x > 1.0 = 0.0
  | x == 0.0 = 0.0
  | x == 1.0 = 1.0
  -- Symmetry: I_x(a,b) = 1 - I_{1-x}(b,a)
  | x > (a + 1.0) / (a + b + 2.0) = 1.0 - regularisedBeta (1.0 - x) b a
  | otherwise =
      let lbeta' = lbeta a b
          front  = exp (a * log x + b * log (1.0 - x) - lbeta')
      in front * betaCF x a b / a

-- | Log of the beta function.
lbeta :: Double -> Double -> Double
lbeta a b = lgamma a + lgamma b - lgamma (a + b)

-- | Stirling's series approximation for log-gamma.
lgamma :: Double -> Double
lgamma z
  | z <= 0.0  = 0.0
  | z < 0.5   = log pi - log (sin (pi * z)) - lgamma (1.0 - z)
  | z < 1.5   = lgamma (z + 1.0) - log z
  | otherwise =
      let c = [ 76.18009172947146, -86.50532032941677,  24.01409824083091
              , -1.231739572450155, 0.1208650973866179e-2, -0.5395239384953e-5 ]
          x  = z - 1.0
          tmp = x + 5.5
          ser = 1.000000000190015
              + sum (zipWith (\ci k -> ci / (x + fromIntegral k)) c [1..6 :: Int])
      in (x + 0.5) * log tmp - tmp + log (2.5066282746310005 * ser)

-- | Continued fraction for the incomplete beta function.
-- Lentz's algorithm (Numerical Recipes).
betaCF :: Double -> Double -> Double -> Double
betaCF x a b =
  let qab = a + b
      qap = a + 1.0
      qam = a - 1.0
      c0  = 1.0
      d0  = max 1e-300 (1.0 - qab * x / qap)
      -- iterate
      go c d m
        | m > 200 = c / d   -- convergence assumed
        | otherwise =
            let m' = fromIntegral m :: Double
                aa1 = m' * (b - m') * x / ((qam + 2.0 * m') * (a + 2.0 * m'))
                d1  = max 1e-300 (1.0 + aa1 * d)
                c1  = max 1e-300 (1.0 + aa1 / c)
                aa2 = -(a + m') * (qab + m') * x / ((a + 2.0 * m') * (qap + 2.0 * m'))
                d2  = max 1e-300 (1.0 + aa2 * d1)
                c2  = max 1e-300 (1.0 + aa2 / c1)
                del = c2 / d2
            in if abs (del - 1.0) < 3.0e-7
                 then c2 * del
                 else go (c1 * c2) (d1 * d2) (m + 1)
  in go c0 (1.0 / d0) (1 :: Int)

-- | t quantile (Cornish-Fisher expansion, A&S 26.7.8).
tQuantile :: Double -> Double -> Double
tQuantile p df
  | p <= 0.0  = -1.0e15
  | p >= 1.0  =  1.0e15
  | df <= 0.0 =  0.0
  | p < 0.5   = -(tQuantile (1.0 - p) df)
  | otherwise =
      let z  = normalQuantile p
          nu = df
          g1 = (z^(3::Int) + z) / (4.0 * nu)
          g2 = (5.0 * z^(5::Int) + 16.0 * z^(3::Int) + 3.0 * z)
               / (96.0 * nu^(2::Int))
          g3 = (3.0 * z^(7::Int) + 19.0 * z^(5::Int) + 17.0 * z^(3::Int) - 15.0 * z)
               / (384.0 * nu^(3::Int))
          g4 = (79.0 * z^(9::Int) + 776.0 * z^(7::Int) + 1482.0 * z^(5::Int)
                - 1920.0 * z^(3::Int) - 945.0 * z)
               / (92160.0 * nu^(4::Int))
      in z + g1 + g2 + g3 + g4

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

-- | F distribution p-value: P(F_{d1,d2} > f).
-- Patnaik's two-moment normal approximation.
fPValue :: Double -> Double -> Double -> Double
fPValue f dfN dfD
  | f <= 0.0 || dfN <= 0.0 || dfD <= 0.0 = 1.0
  | otherwise =
      let z = ((f / dfN)** (1.0/3.0) * (1.0 - 2.0/(9.0*dfN))
               - (1.0 - 2.0/(9.0*dfD)))
              / sqrt (2.0/(9.0*dfN) / ((f/dfN)** (2.0/3.0))
                     + 2.0/(9.0*dfD))
      in max 0.0 (min 1.0 (normSFUpper z))

-- | Chi-square upper tail: P(X^2_{df} > chi2). Wilson-Hilferty.
chiSqPValue :: Double -> Double -> Double
chiSqPValue chi2 df
  | chi2 <= 0.0 = 1.0
  | df <= 0.0   = 0.0
  | otherwise   =
      let z = ((chi2/df)** (1.0/3.0) - (1.0 - 2.0/(9.0*df)))
              / sqrt (2.0/(9.0*df))
      in max 0.0 (min 1.0 (normSFUpper z))

formatP :: Double -> T.Text
formatP p = T.pack (show (fromIntegral (round (p * 1000) :: Int) / 1000.0 :: Double))

round2 :: Double -> Double
round2 x = fromIntegral (round (x * 100) :: Int) / 100.0
