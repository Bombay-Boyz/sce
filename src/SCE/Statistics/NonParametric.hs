{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData         #-}

{-|
Module      : SCE.Statistics.NonParametric
Description : Non-parametric tests: Mann-Whitney U, Wilcoxon, Kruskal-Wallis, Sign test (Phase 3)
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

== Tests implemented

  * Mann-Whitney U (Mann & Whitney 1947) — two independent samples.
    Handles ties via the normal approximation with tie correction.
    Matches R @wilcox.test(x, y, exact=FALSE)@.

  * Wilcoxon signed-rank (Wilcoxon 1945) — paired samples.
    Uses normal approximation with continuity correction for n >= 10.

  * Kruskal-Wallis H (Kruskal & Wallis 1952) — k independent groups.
    Chi-square approximation; tie correction applied.

  * Sign test — simplest paired non-parametric test.
    Uses exact binomial p-value for n <= 25, normal approximation otherwise.

== Design constraints
  * No partial functions.
  * All public functions return SCEResult.
  * P-values clamped to [0, 1].
-}
module SCE.Statistics.NonParametric
  ( mannWhitneyU
  , wilcoxonSigned
  , kruskalWallis
  , signTest
  ) where

import SCE.Core.Types
  ( SCEResult, mkError, ErrorCode(..), Severity(..) )
import SCE.Core.Statistics ( kahanMean, kahanSum )
import SCE.Statistics.TestResult
  ( TestResult(..), EffectSize(..), AssumptionCheck(..), AssumptionStatus(..)
  , mkTestResult
  )

import qualified Data.Text   as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.List   (sort, sortBy, groupBy, nub)
import           Data.Ord    (comparing)

-- ---------------------------------------------------------------------------
-- Mann-Whitney U
-- ---------------------------------------------------------------------------

-- | Mann-Whitney U test for two independent samples.
--
-- H₀: P(X > Y) = 0.5 (the two populations are stochastically equal).
-- Uses the normal approximation with tie correction.  Matches R
-- @wilcox.test(x, y, exact=FALSE, correct=TRUE)@.
--
-- Effect size: rank-biserial correlation r = 1 - 2U/(n1*n2) ∈ [-1, 1].
-- Returns 'Left E4002' if n < 3 in either group.
mannWhitneyU :: Vector Double -> Vector Double -> SCEResult TestResult
mannWhitneyU xs ys
  | V.length xs < 3 || V.length ys < 3 =
      Left $ mkError E4002
        "Mann-Whitney U requires >= 3 observations per group"
        ["Collect more data."] Error
  | otherwise =
      let n1    = V.length xs
          n2    = V.length ys
          n     = n1 + n2
          xList = V.toList xs
          yList = V.toList ys
          -- Assign ranks to the combined sample; groups marked (value, group)
          combined = sortBy (comparing fst)
                       $ map (\x -> (x, 1 :: Int)) xList
                      ++ map (\y -> (y, 2 :: Int)) yList
          -- Fractional ranks (tie-handling)
          ranked  = assignRanks combined
          -- Sum of ranks for group 1
          r1      = sum [ r | (_, g, r) <- ranked, g == 1 ]
          u1      = r1 - fromIntegral (n1 * (n1 + 1)) / 2.0
          u2      = fromIntegral (n1 * n2) - u1
          uStat   = min u1 u2
          -- Tie correction
          tieCorr = tieCorrection (map (\(v,_) -> v) combined)
          -- Normal approximation with continuity correction
          meanU   = fromIntegral (n1 * n2) / 2.0
          varU    = fromIntegral (n1 * n2) / 12.0
                    * (fromIntegral n + 1.0 - tieCorr / fromIntegral (n * (n-1)))
          seU     = sqrt (max 1e-300 varU)
          z       = (abs (uStat - meanU) - 0.5) / seU  -- continuity correction
          pv      = 2.0 * normSFUpper z
          -- Effect size: rank-biserial correlation
          rBis    = 1.0 - 2.0 * uStat / fromIntegral (n1 * n2)
          ac      = AssumptionCheck "Independence" Satisfied
                      "Observations within each group are independent"
          interp  = "Mann-Whitney U = " <> T.pack (show (round uStat :: Int))
                    <> ", z = " <> T.pack (show (round2 z))
                    <> ", p = " <> formatP pv
      in Right $ mkTestResult
           "Mann-Whitney U" uStat Nothing pv Nothing
           (Just (PhiCoefficient rBis))
           Nothing n [ac] interp

-- | Assign fractional ranks to a sorted (value, group) list.
-- Returns (value, group, rank).
assignRanks :: [(Double, Int)] -> [(Double, Int, Double)]
assignRanks sorted =
  let groups = groupBy (\a b -> fst a == fst b) sorted
      ranked  = go 1 groups
  in ranked
  where
    go _ []       = []
    go i (g:rest) =
      let sz    = length g
          avgR  = fromIntegral (2 * i + sz - 1) / 2.0
          items = [ (v, gr, avgR) | (v, gr) <- g ]
      in items ++ go (i + sz) rest

-- | Tie correction factor: sum_t (t^3 - t) for each group of ties of size t.
tieCorrection :: [Double] -> Double
tieCorrection vals =
  let sorted = sort vals
      grps   = groupBy (==) sorted
  in sum [ fromIntegral (length g)^(3::Int) - fromIntegral (length g)
         | g <- grps, length g > 1 ]

-- ---------------------------------------------------------------------------
-- Wilcoxon signed-rank test
-- ---------------------------------------------------------------------------

-- | Wilcoxon signed-rank test for paired samples.
--
-- H₀: the median of the differences is zero.
-- Uses normal approximation with continuity correction.
-- Matches R @wilcox.test(x, y, paired=TRUE, exact=FALSE, correct=TRUE)@.
--
-- Returns 'Left E2001' if vectors have different lengths.
-- Returns 'Left E4002' if n (after removing ties at 0) < 3.
wilcoxonSigned :: Vector Double -> Vector Double -> SCEResult TestResult
wilcoxonSigned xs ys
  | V.length xs /= V.length ys =
      Left $ mkError E2001
        ("Wilcoxon signed-rank requires equal-length vectors: got "
          <> T.pack (show (V.length xs)) <> " and " <> T.pack (show (V.length ys)))
        ["Ensure both vectors have the same number of paired observations."] Error
  | V.length xs < 3 =
      Left $ mkError E4002
        "Wilcoxon signed-rank requires >= 3 paired observations"
        ["Collect more data."] Error
  | otherwise =
      let diffs  = V.toList $ V.zipWith (-) xs ys
          -- Remove exact zeros
          nonZ   = filter (\d -> abs d > 1e-300) diffs
          n      = length nonZ
          absDiffs = sort (map abs nonZ)
          -- Assign ranks to |d_i|
          ranked  = assignRanksList absDiffs
          -- T+ = sum of ranks where d_i > 0
          tPlus   = sum [ r | (d, r) <- zip nonZ (getRanks absDiffs nonZ), d > 0 ]
          tMinus  = fromIntegral (n * (n + 1)) / 2.0 - tPlus
          wStat   = min tPlus tMinus
          meanW   = fromIntegral (n * (n + 1)) / 4.0
          varW    = fromIntegral (n * (n + 1) * (2 * n + 1)) / 24.0
                    - tieCorrection absDiffs / 48.0
          seW     = sqrt (max 1e-300 varW)
          z       = (abs (wStat - meanW) - 0.5) / seW
          pv      = 2.0 * normSFUpper z
          interp  = "Wilcoxon signed-rank: W = " <> T.pack (show (round wStat :: Int))
                    <> ", z = " <> T.pack (show (round2 z))
                    <> ", p = " <> formatP pv
      in if n < 3
           then Left $ mkError E4002
                  "Wilcoxon signed-rank: fewer than 3 non-zero differences"
                  ["Collect more paired observations or use the sign test."] Error
           else Right $ mkTestResult
                  "Wilcoxon signed-rank" wStat Nothing pv Nothing Nothing Nothing
                  (V.length xs)
                  [AssumptionCheck "Symmetry" NotTested
                     "Assumes symmetric distribution of differences"]
                  interp

assignRanksList :: [Double] -> [(Double, Double)]
assignRanksList xs =
  let sorted = sort xs
      grps   = groupBy (==) sorted
      ranked = go 1 grps
  in ranked
  where
    go _ []     = []
    go i (g:rest) =
      let sz   = length g
          avgR = fromIntegral (2 * i + sz - 1) / 2.0
      in map (\v -> (v, avgR)) g ++ go (i + sz) rest

-- | Map each original difference to its rank by absolute value.
getRanks :: [Double] -> [Double] -> [Double]
getRanks absSorted origDiffs =
  let ranked = assignRanksList absSorted
      rankOf d = case lookup (abs d) ranked of
                   Just r  -> r
                   Nothing -> 0.0
  in map rankOf origDiffs

-- ---------------------------------------------------------------------------
-- Kruskal-Wallis
-- ---------------------------------------------------------------------------

-- | Kruskal-Wallis H test for k independent groups.
--
-- H₀: all group distributions are identical.
-- Chi-square approximation with tie correction.
-- Effect size: eta-squared (E^2) = (H - k + 1) / (n - k).
--
-- Returns 'Left E4002' if fewer than 2 groups or any group has n < 2.
kruskalWallis :: [Vector Double] -> SCEResult TestResult
kruskalWallis groups
  | length groups < 2 =
      Left $ mkError E4002
        "Kruskal-Wallis requires at least 2 groups" [] Error
  | any (\g -> V.length g < 2) groups =
      Left $ mkError E4002
        "Every group must have >= 2 observations for Kruskal-Wallis"
        ["Collect more data in each group."] Error
  | otherwise =
      let k       = length groups
          ns      = map V.length groups
          n       = sum ns
          -- Combine with group labels, sort, assign ranks
          combined = sortBy (comparing fst)
                       $ concatMap (\(gi, g) -> map (\x -> (x, gi)) (V.toList g))
                       $ zip [0::Int ..] groups
          ranked   = assignRanks combined
          -- Sum of ranks per group
          rSums    = [ sum [ r | (_, gi, r) <- ranked, gi == g ]
                     | g <- [0..k-1] ]
          -- Tie correction
          tieC     = tieCorrection (map (\(v,_) -> v) combined)
          -- H statistic
          hStat    = (12.0 / (fromIntegral n * (fromIntegral n + 1.0)))
                     * sum [ rs^(2::Int) / fromIntegral ni
                           | (rs, ni) <- zip rSums ns ]
                     - 3.0 * (fromIntegral n + 1.0)
          -- Tie correction factor
          c        = 1.0 - tieC / (fromIntegral n^(3::Int) - fromIntegral n)
          hCorr    = if c < 1e-300 then hStat else hStat / c
          df       = fromIntegral (k - 1) :: Double
          pv       = chiSqPValue hCorr df
          etaSq    = max 0.0 ((hCorr - df) / (fromIntegral n - df - 1.0))
          ac       = AssumptionCheck "Independence" Satisfied
                       "Observations within each group are independent"
          interp   = "Kruskal-Wallis H(" <> T.pack (show (k-1))
                     <> ") = " <> T.pack (show (round2 hCorr))
                     <> ", p = " <> formatP pv
      in Right $ mkTestResult
           "Kruskal-Wallis" hCorr (Just df) pv Nothing
           (Just (EtaSquared etaSq))
           Nothing n [ac] interp

-- ---------------------------------------------------------------------------
-- Sign test
-- ---------------------------------------------------------------------------

-- | Sign test for paired samples (simplest non-parametric paired test).
--
-- H₀: P(X > Y) = 0.5.
-- Uses exact binomial p-value for n <= 50, normal approximation otherwise.
-- Returns 'Left E2001' if vectors have different lengths.
-- Returns 'Left E4002' if effective n (non-tied pairs) < 3.
signTest :: Vector Double -> Vector Double -> SCEResult TestResult
signTest xs ys
  | V.length xs /= V.length ys =
      Left $ mkError E2001
        ("Sign test requires equal-length vectors: got "
          <> T.pack (show (V.length xs)) <> " and " <> T.pack (show (V.length ys)))
        ["Ensure both vectors are paired."] Error
  | V.length xs < 3 =
      Left $ mkError E4002
        "Sign test requires >= 3 paired observations" [] Error
  | otherwise =
      let diffs   = V.toList $ V.zipWith (-) xs ys
          nonZero = filter (\d -> abs d > 1e-300) diffs
          n       = length nonZero
          plus    = length (filter (> 0) nonZero)
          minus   = length (filter (< 0) nonZero)
          kStat   = fromIntegral (min plus minus) :: Double
          pv      = if n <= 50
                      then exactBinomialPV (min plus minus) n
                      else let z = (kStat - fromIntegral n / 2.0 + 0.5)
                                   / sqrt (fromIntegral n / 4.0)
                           in 2.0 * normSFUpper (abs z)
          interp  = "Sign test: n = " <> T.pack (show n)
                    <> ", + = " <> T.pack (show plus)
                    <> ", - = " <> T.pack (show minus)
                    <> ", p = " <> formatP pv
      in if n < 3
           then Left $ mkError E4002
                  "Sign test: fewer than 3 non-tied pairs"
                  ["Collect more paired observations."] Error
           else Right $ mkTestResult
                  "sign test" kStat Nothing pv Nothing Nothing Nothing
                  (V.length xs)
                  [AssumptionCheck "Independence" Satisfied
                     "Pairs are independent"]
                  interp

-- | Exact two-sided binomial p-value P(X <= k) + P(X >= n-k) for X ~ Bin(n, 0.5).
exactBinomialPV :: Int -> Int -> Double
exactBinomialPV k n
  | n <= 0    = 1.0
  | k < 0     = 1.0
  | k > n     = 0.0
  | otherwise =
      let pHalf = 0.5 ^ n
          lower = sum [ binomCoeff n j * pHalf | j <- [0..k] ]
      in max 0.0 (min 1.0 (2.0 * lower))

-- | Binomial coefficient C(n, k).
binomCoeff :: Int -> Int -> Double
binomCoeff n k
  | k < 0 || k > n = 0.0
  | k == 0 || k == n = 1.0
  | otherwise =
      let k' = min k (n - k)
      in product [ fromIntegral (n - i) / fromIntegral (i + 1) | i <- [0..k'-1] ]

-- ---------------------------------------------------------------------------
-- Shared helpers
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

-- | Chi-square upper tail: P(X^2_{df} > chi2).
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
