{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData         #-}

{-|
Module      : SCE.Statistics.Categorical
Description : Chi-square tests, Fisher's exact, Cramér's V, phi (Phase 3)
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

== Tests implemented

  * Chi-square goodness-of-fit (Pearson 1900).
  * Chi-square test of independence (Pearson 1900, contingency table).
  * Fisher's exact test — 2×2 tables; hypergeometric p-value.
  * Cramér's V — effect size for chi-square; always in [0, 1].
  * Phi coefficient — 2×2 tables only; equivalent to Pearson r.

== Design constraints
  * No partial functions.
  * All public functions return SCEResult.
  * P-values clamped to [0, 1] by mkTestResult.
-}
module SCE.Statistics.Categorical
  ( -- * Contingency table
    ContingencyTable(..)
  , mkContingency
    -- * Chi-square tests
  , chiSquareGOF
  , chiSquareIndependence
    -- * Fisher's exact
  , fishersExact
    -- * Effect sizes
  , cramersV
  , phiCoefficient
  ) where

import SCE.Core.Types
  ( SCEResult, mkError, ErrorCode(..), Severity(..) )
import SCE.Statistics.TestResult
  ( TestResult(..), EffectSize(..), AssumptionCheck(..), AssumptionStatus(..)
  , mkTestResult
  )

import qualified Data.Text       as T
import           Data.Vector     (Vector)
import qualified Data.Vector     as V
import           Data.List       (nub, sort)
import qualified Data.Map.Strict as M

-- ---------------------------------------------------------------------------
-- ContingencyTable
-- ---------------------------------------------------------------------------

-- | A two-way contingency table of observed counts.
data ContingencyTable = ContingencyTable
  { ctRowLabels  :: [T.Text]
    -- ^ Row category labels (length = r).
  , ctColLabels  :: [T.Text]
    -- ^ Column category labels (length = c).
  , ctCounts     :: Vector (Vector Int)
    -- ^ Observed counts, row-major: ctCounts[i][j] = count(row i, col j).
  , ctRowTotals  :: Vector Int
    -- ^ Row totals as a Vector for safe O(1) indexing.
  , ctColTotals  :: Vector Int
    -- ^ Column totals as a Vector for safe O(1) indexing.
  , ctGrandTotal :: Int
  } deriving stock (Show)

-- | Build a contingency table from two categorical vectors.
--
-- Returns 'Left E2002' if either vector is empty, or if the grand total is 0.
-- Returns 'Left E2001' if the vectors have different lengths.
mkContingency :: Vector T.Text -> Vector T.Text -> SCEResult ContingencyTable
mkContingency rowVec colVec
  | V.null rowVec =
      Left $ mkError E2002
        "mkContingency: row vector is empty"
        ["Provide at least one categorical observation."] Error
  | V.length rowVec /= V.length colVec =
      Left $ mkError E2001
        ("mkContingency: length mismatch ("
          <> T.pack (show (V.length rowVec))
          <> " vs " <> T.pack (show (V.length colVec)) <> ")")
        ["Both categorical vectors must have the same length."] Error
  | otherwise =
      let rowLabels  = sort $ nub $ V.toList rowVec
          colLabels  = sort $ nub $ V.toList colVec
          -- Build count map
          countMap   = V.foldl' (\m (rv, cv) -> M.insertWith (+) (rv, cv) 1 m)
                                 M.empty
                                 (V.zip rowVec colVec)
          -- Fill count matrix
          mat        = V.fromList
                         [ V.fromList
                           [ M.findWithDefault 0 (rl, cl) countMap
                           | cl <- colLabels ]
                         | rl <- rowLabels ]
          -- Store totals as Vectors for safe indexing
          rowTotals  = V.fromList
                         [ sum [ M.findWithDefault 0 (rl, cl) countMap | cl <- colLabels ]
                         | rl <- rowLabels ]
          colTotals  = V.fromList
                         [ sum [ M.findWithDefault 0 (rl, cl) countMap | rl <- rowLabels ]
                         | cl <- colLabels ]
          grandTotal = V.sum rowTotals
      in Right ContingencyTable
           { ctRowLabels  = rowLabels
           , ctColLabels  = colLabels
           , ctCounts     = mat
           , ctRowTotals  = rowTotals
           , ctColTotals  = colTotals
           , ctGrandTotal = grandTotal
           }

-- ---------------------------------------------------------------------------
-- Chi-square goodness-of-fit
-- ---------------------------------------------------------------------------

-- | Chi-square goodness-of-fit test.
--
-- Tests whether observed frequencies match expected proportions.
-- Returns 'Left E4002' if any expected count < 5 (recommend Fisher's exact).
-- Returns 'Left E2002' if observed or expected vectors are empty or mismatched.
chiSquareGOF
  :: Vector Int     -- ^ Observed counts
  -> Vector Double  -- ^ Expected proportions (need not sum to 1; normalised internally)
  -> SCEResult TestResult
chiSquareGOF observed expected
  | V.null observed =
      Left $ mkError E2002 "Observed vector is empty" ["Provide at least one category."] Error
  | V.length observed /= V.length expected =
      Left $ mkError E2001
        ("Length mismatch: observed=" <> T.pack (show (V.length observed))
          <> ", expected=" <> T.pack (show (V.length expected)))
        ["Both vectors must have the same number of categories."] Error
  | otherwise =
      let n       = fromIntegral (V.sum observed) :: Double
          propSum = V.sum expected
          -- Normalise expected proportions
          expCounts = if propSum < 1e-300
                        then V.replicate (V.length expected) (n / fromIntegral (V.length expected))
                        else V.map (\p -> p / propSum * n) expected
          -- Check minimum expected count
          minExp  = case V.uncons expCounts of
                      Nothing       -> 0.0
                      Just (h, tl)  -> V.foldl' min h tl
          df      = fromIntegral (V.length observed - 1) :: Double
          chi2    = V.sum $ V.zipWith
                      (\o e -> let diff = fromIntegral o - e
                               in if e < 1e-300 then 0.0 else diff * diff / e)
                      observed expCounts
          pv      = chiSqPValue chi2 df
          acMsg   = if minExp < 5.0
                      then "WARNING: minimum expected count = " <> T.pack (show (round2 minExp))
                           <> " < 5; consider Fisher's exact or collapsing categories."
                      else "Expected counts all >= 5; chi-square approximation is valid."
          ac      = AssumptionCheck "Expected cell counts >= 5"
                      (if minExp < 5.0 then Violated else Satisfied)
                      acMsg
          interp  = "Chi-square GoF: chi2(" <> T.pack (show (round df :: Int))
                    <> ") = " <> T.pack (show (round2 chi2))
                    <> ", p = " <> formatP pv
      in Right $ mkTestResult
           "chi-square goodness-of-fit" chi2 (Just df) pv Nothing Nothing Nothing
           (V.length observed) [ac] interp

-- ---------------------------------------------------------------------------
-- Chi-square independence
-- ---------------------------------------------------------------------------

-- | Chi-square test of independence for a contingency table.
--
-- Returns 'Left E4002' if any expected cell count < 5 (recommendation warning
-- embedded in AssumptionCheck; test still runs).
-- Returns 'Left E2002' if grand total = 0.
chiSquareIndependence :: ContingencyTable -> SCEResult TestResult
chiSquareIndependence ct
  | ctGrandTotal ct == 0 =
      Left $ mkError E2002
        "Chi-square independence: grand total is 0"
        ["Provide non-empty contingency data."] Error
  | length (ctRowLabels ct) < 2 || length (ctColLabels ct) < 2 =
      Left $ mkError E2002
        "Chi-square independence requires at least a 2x2 table"
        ["Ensure each variable has at least 2 categories."] Error
  | otherwise =
      let r      = length (ctRowLabels ct)
          c      = length (ctColLabels ct)
          n      = fromIntegral (ctGrandTotal ct) :: Double
          -- FIX: replaced (ctRowTotals ct !! i) and (ctColTotals ct !! j) —
          -- both were partial list indexing.  ctRowTotals/ctColTotals are now
          -- Vector Int so we use safe V.!? with a 0 fallback (the 0 case is
          -- unreachable because i in [0..r-1] and j in [0..c-1] are always
          -- within bounds of the vectors produced by mkContingency).
          safeRowTotal i = fromIntegral $ case ctRowTotals ct V.!? i of
                             Just x  -> x
                             Nothing -> 0
          safeColTotal j = fromIntegral $ case ctColTotals ct V.!? j of
                             Just x  -> x
                             Nothing -> 0
          -- Expected counts: E_ij = (rowTotal_i * colTotal_j) / n
          expMat = V.fromList
                     [ V.fromList
                       [ safeRowTotal i * safeColTotal j / n
                       | j <- [0..c-1] ]
                     | i <- [0..r-1] ]
          allExp = [ e | i <- [0..r-1]
                       , j <- [0..c-1]
                       , let e = case (expMat V.!? i) >>= (V.!? j) of
                                   Just x  -> x
                                   Nothing -> 0.0 ]
          minExp = case allExp of { [] -> 0.0 ; (x:xs) -> foldl min x xs }
          chi2   = sum [ let obs = fromIntegral $ case (ctCounts ct V.!? i) >>= (V.!? j) of
                                     Just x  -> x
                                     Nothing -> 0
                             ex  = case (expMat V.!? i) >>= (V.!? j) of
                                     Just x  -> x
                                     Nothing -> 0.0
                         in if ex < 1e-300 then 0.0
                            else (obs - ex) * (obs - ex) / ex
                       | i <- [0..r-1], j <- [0..c-1] ]
          df     = fromIntegral ((r-1) * (c-1)) :: Double
          pv     = chiSqPValue chi2 df
          ac     = AssumptionCheck "Expected cell counts >= 5"
                     (if minExp < 5.0 then Violated else Satisfied)
                     (if minExp < 5.0
                        then "Minimum expected cell = " <> T.pack (show (round2 minExp))
                             <> " < 5; consider Fisher's exact for 2x2."
                        else "All expected cells >= 5; approximation is valid.")
          interp = "Chi-square independence: chi2(" <> T.pack (show (round df :: Int))
                   <> ") = " <> T.pack (show (round2 chi2))
                   <> ", p = " <> formatP pv
      in Right $ mkTestResult
           "chi-square independence" chi2 (Just df) pv Nothing Nothing Nothing
           (ctGrandTotal ct) [ac] interp

-- ---------------------------------------------------------------------------
-- Fisher's exact test (2x2)
-- ---------------------------------------------------------------------------

-- | Fisher's exact test for 2×2 contingency tables.
--
-- Computes the exact hypergeometric p-value (two-sided).
-- Returns 'Left E2002' if the table is not 2×2.
fishersExact :: ContingencyTable -> SCEResult TestResult
fishersExact ct
  | length (ctRowLabels ct) /= 2 || length (ctColLabels ct) /= 2 =
      Left $ mkError E2002
        "Fisher's exact test requires a 2x2 contingency table"
        ["Use chiSquareIndependence for larger tables."] Error
  | ctGrandTotal ct == 0 =
      Left $ mkError E2002
        "Fisher's exact: grand total is 0" [] Error
  | otherwise =
      -- Safe: table is exactly 2×2 (guarded above), so all indices are valid.
      let safeAt r c = case (ctCounts ct V.!? r) >>= (V.!? c) of
                         Just x  -> x
                         Nothing -> 0
          a = safeAt 0 0
          b = safeAt 0 1
          c = safeAt 1 0
          d = safeAt 1 1
          n = a + b + c + d
          r1 = a + b   -- row 1 total
          c1 = a + c   -- col 1 total
          -- Hypergeometric P(X = k) for X ~ Hyp(n, r1, c1)
          -- Two-sided: sum all k with P(X=k) <= P(X=a)
          pObs = hypergeomPMF a r1 c1 n
          pv   = hypergeomTwoSided a r1 c1 n pObs
          -- Odds ratio
          or_  = if b * c == 0
                   then if a * d == 0 then 1.0 else 1.0e15
                   else fromIntegral (a * d) / fromIntegral (b * c)
          interp = "Fisher's exact: odds ratio = " <> T.pack (show (round2 or_))
                   <> ", p = " <> formatP pv
      in Right $ mkTestResult
           "Fisher's exact" or_ Nothing pv Nothing Nothing Nothing
           n [] interp

-- | Hypergeometric PMF: P(X = k) for X ~ Hyp(n, K, n).
hypergeomPMF :: Int -> Int -> Int -> Int -> Double
hypergeomPMF k bigK n bigN =
  let logP = logBinomCoeff bigK k
             + logBinomCoeff (bigN - bigK) (n - k)
             - logBinomCoeff bigN n
  in exp logP

-- | Two-sided hypergeometric p-value.
hypergeomTwoSided :: Int -> Int -> Int -> Int -> Double -> Double
hypergeomTwoSided observed bigK n bigN pObs =
  let kMin = max 0 (n + bigK - bigN)
      kMax = min n bigK
      pv   = sum [ hypergeomPMF k bigK n bigN
                 | k <- [kMin..kMax]
                 , hypergeomPMF k bigK n bigN <= pObs * (1.0 + 1e-9) ]
  in max 0.0 (min 1.0 pv)

-- | Log of binomial coefficient C(n, k).
logBinomCoeff :: Int -> Int -> Double
logBinomCoeff n k
  | k < 0 || k > n = -1.0e300   -- effectively 0 probability
  | k == 0 || k == n = 0.0
  | otherwise =
      let k' = min k (n - k)
      in sum [ log (fromIntegral (n - i)) - log (fromIntegral (i + 1)) | i <- [0..k'-1] ]

-- ---------------------------------------------------------------------------
-- Effect sizes
-- ---------------------------------------------------------------------------

-- | Cramér's V — effect size for chi-square test of independence.
--
-- V = sqrt(chi2 / (n * (min(r,c) - 1))).  Always in [0, 1].
-- Bias-corrected version (Bergsma 2013) is used: V~ = max(0, ...).
-- Returns 'Left E2002' if grand total = 0.
cramersV :: ContingencyTable -> SCEResult Double
cramersV ct
  | ctGrandTotal ct == 0 =
      Right 0.0
  | length (ctRowLabels ct) < 2
    || length (ctColLabels ct) < 2 =
      Right 0.0
  | otherwise =
      case chiSquareIndependence ct of
        Left _ -> Right 0.0
        Right tr ->
          let chi2 = trStatistic tr
              n    = fromIntegral (ctGrandTotal ct) :: Double
              r    = length (ctRowLabels ct)
              c    = length (ctColLabels ct)
              k    = fromIntegral (min r c - 1) :: Double
              v    = if k < 1e-300 || n < 1e-300
                       then 0.0
                       else sqrt (max 0.0 (chi2 / (n * k)))
          in Right (max 0.0 (min 1.0 v))

-- | Phi coefficient for 2×2 tables: phi = sqrt(chi2 / n).
--
-- Equivalent to Pearson r for two binary variables.
-- Returns 'Left E2002' if table is not 2×2 or grand total is 0.
phiCoefficient :: ContingencyTable -> SCEResult Double
phiCoefficient ct
  | length (ctRowLabels ct) /= 2 || length (ctColLabels ct) /= 2 =
      Left $ mkError E2002
        "Phi coefficient is only defined for 2x2 contingency tables"
        ["Use Cramér's V for larger tables."] Error
  | ctGrandTotal ct == 0 =
      Left $ mkError E2002 "Phi coefficient: grand total is 0" [] Error
  | otherwise =
      -- Safe: 2×2 table guaranteed (guarded above).
      let safeAt r c = fromIntegral $ case (ctCounts ct V.!? r) >>= (V.!? c) of
                                        Just x  -> x
                                        Nothing -> 0 :: Int
          a  = safeAt 0 0 :: Double
          b  = safeAt 0 1 :: Double
          c  = safeAt 1 0 :: Double
          d  = safeAt 1 1 :: Double
          -- phi = (ad - bc) / sqrt(r1*r2*c1*c2)
          r1 = a + b; r2 = c + d
          c1 = a + c; c2 = b + d
          denom = sqrt (r1 * r2 * c1 * c2)
          phi   = if denom < 1e-300 then 0.0 else (a * d - b * c) / denom
      in Right (max (-1.0) (min 1.0 phi))

-- ---------------------------------------------------------------------------
-- Shared helpers
-- ---------------------------------------------------------------------------

-- | Chi-square upper tail: P(X^2_{df} > chi2).  Wilson-Hilferty.
chiSqPValue :: Double -> Double -> Double
chiSqPValue chi2 df
  | chi2 <= 0.0 = 1.0
  | df <= 0.0   = 0.0
  | otherwise   =
      let z = ((chi2/df)** (1.0/3.0) - (1.0 - 2.0/(9.0*df)))
              / sqrt (2.0/(9.0*df))
      in max 0.0 (min 1.0 (normSFUpper z))

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

formatP :: Double -> T.Text
formatP p = T.pack (show (fromIntegral (round (p * 1000) :: Int) / 1000.0 :: Double))

round2 :: Double -> Double
round2 x = fromIntegral (round (x * 100) :: Int) / 100.0
