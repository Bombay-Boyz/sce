{-# LANGUAGE StrictData        #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

{-|
Module      : SCE.Core.Correlation
Description : Pearson, Spearman, Kendall τ, partial correlation, covariance (Phase 2.3)
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

== Algorithms

  * Pearson  : numerically stable via Welford-style two-pass; result clamped to [-1,1].
  * Spearman : convert each vector to fractional ranks (handles ties by averaging),
               then compute Pearson on the rank vectors.
  * Kendall τ: O(n²) concordance/discordance count with correct tie handling
               (τ-b formula — matches R's @cor(method="kendall")@).
  * Partial   : first-order partial r via the standard formula
               r_xy.z = (r_xy − r_xz·r_yz) / sqrt((1−r_xz²)(1−r_yz²))
               Extended to higher orders by iterative application.
  * Covariance: sample covariance using Kahan sums for numerical stability.
  * Matrix    : all pairwise Pearson correlations for a list of vectors.

== Design constraints
  * NO partial functions.
  * All public functions return 'SCEResult'.
  * Pearson result is always clamped to [-1, 1] (floating-point rounding guard).
  * Vectors must have identical length (>= minimum requirement per function).

== Liquid Haskell annotations (stubs)

{-@ type Correlation = {v:Double | -1 <= v && v <= 1}  @-}

{-@ pearsonCorrelation
      :: xs:{v:V.Vector Double | vlen v >= 3}
      -> ys:{v:V.Vector Double | vlen v = vlen xs}
      -> SCEResult Correlation                          @-}
-}
module SCE.Core.Correlation
  ( -- * Pearson
    pearsonCorrelation
  , covariance

    -- * Spearman
  , spearmanCorrelation

    -- * Kendall τ-b
  , kendallTau

    -- * Partial correlation
  , partialCorrelation

    -- * Correlation matrix
  , CorrelationMatrix(..)
  , correlationMatrix
  , lookupCorrelation

    -- * Result type
  , CorrelationResult(..)
  ) where

import SCE.Core.Types
  ( SCEResult, mkError, ErrorCode(..), Severity(..) )
import SCE.Core.Statistics
  ( kahanSum, kahanMean, welfordVariance )

import qualified Data.Text   as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import           Data.List   (sortBy, groupBy, nub, sort)
import           Data.Ord    (comparing)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-- ---------------------------------------------------------------------------
-- Result type
-- ---------------------------------------------------------------------------

-- | A correlation with an asymptotic p-value and confidence interval.
-- p-value uses Fisher's z-transformation (approximate for n >= 10).
data CorrelationResult = CorrelationResult
  { corrValue  :: Double        -- ^ Correlation coefficient in [-1, 1]
  , corrPValue :: Maybe Double  -- ^ Two-sided p-value (Nothing when n < 10)
  , corrCI95Lo :: Maybe Double  -- ^ 95% CI lower bound
  , corrCI95Hi :: Maybe Double  -- ^ 95% CI upper bound
  , corrN      :: Int           -- ^ Sample size used
  } deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Pearson correlation
-- ---------------------------------------------------------------------------

-- | Sample Pearson product-moment correlation coefficient.
--
-- Uses a two-pass numerically stable algorithm:
-- pass 1 — Kahan mean; pass 2 — Welford-style centred products.
-- Result is clamped to [-1, 1] to guard against floating-point rounding.
--
-- Minimum sample size: 3 (to distinguish from a trivially perfect fit).
pearsonCorrelation :: Vector Double -> Vector Double -> SCEResult CorrelationResult
pearsonCorrelation xs ys
  | V.length xs < 3       = Left $ mkError E2002
      ("Pearson correlation requires >= 3 observations, got "
        <> T.pack (show (V.length xs)))
      ["Collect more data."] Error
  | V.length xs /= V.length ys = Left $ mkError E2001
      ("Vector length mismatch: xs=" <> T.pack (show (V.length xs))
        <> ", ys=" <> T.pack (show (V.length ys)))
      ["Both vectors must have the same number of observations."] Error
  | otherwise =
      let r = clamp11 (rawPearson xs ys)
      in Right $ CorrelationResult
           { corrValue  = r
           , corrPValue = fisherPValue r (V.length xs)
           , corrCI95Lo = fmap fst (fisherCI95 r (V.length xs))
           , corrCI95Hi = fmap snd (fisherCI95 r (V.length xs))
           , corrN      = V.length xs
           }

-- | Sample covariance using Kahan sums for stability.
covariance :: Vector Double -> Vector Double -> SCEResult Double
covariance xs ys
  | V.length xs < 2       = Left $ mkError E2002
      ("Covariance requires >= 2 observations, got "
        <> T.pack (show (V.length xs)))
      ["Collect more data."] Error
  | V.length xs /= V.length ys = Left $ mkError E2001
      ("Vector length mismatch for covariance: xs="
        <> T.pack (show (V.length xs))
        <> ", ys=" <> T.pack (show (V.length ys)))
      ["Both vectors must have the same number of observations."] Error
  | otherwise =
      let n    = fromIntegral (V.length xs) :: Double
          mx   = kahanMean xs
          my   = kahanMean ys
          cov  = kahanSum (V.zipWith (\x y -> (x - mx) * (y - my)) xs ys)
                 / (n - 1.0)
      in Right cov

-- ---------------------------------------------------------------------------
-- Spearman rank correlation
-- ---------------------------------------------------------------------------

-- | Spearman rank correlation coefficient.
--
-- Converts each vector to fractional (mid) ranks, handling ties by
-- averaging equal values, then computes Pearson on the rank vectors.
spearmanCorrelation :: Vector Double -> Vector Double -> SCEResult CorrelationResult
spearmanCorrelation xs ys
  | V.length xs < 3       = Left $ mkError E2002
      ("Spearman correlation requires >= 3 observations, got "
        <> T.pack (show (V.length xs)))
      ["Collect more data."] Error
  | V.length xs /= V.length ys = Left $ mkError E2001
      ("Vector length mismatch for Spearman: xs="
        <> T.pack (show (V.length xs))
        <> ", ys=" <> T.pack (show (V.length ys)))
      ["Both vectors must have the same number of observations."] Error
  | otherwise =
      let rxs = fractionalRanks xs
          rys = fractionalRanks ys
          r   = clamp11 (rawPearson rxs rys)
      in Right $ CorrelationResult
           { corrValue  = r
           , corrPValue = fisherPValue r (V.length xs)
           , corrCI95Lo = fmap fst (fisherCI95 r (V.length xs))
           , corrCI95Hi = fmap snd (fisherCI95 r (V.length xs))
           , corrN      = V.length xs
           }

-- ---------------------------------------------------------------------------
-- Kendall τ-b
-- ---------------------------------------------------------------------------

-- | Kendall τ-b correlation.
--
-- O(n²) concordance/discordance counting with tie correction.
-- τ-b formula (matches R's @cor(method="kendall")@):
--
-- @τ-b = (C - D) / sqrt((C + D + T_x)(C + D + T_y))@
--
-- where C = concordant pairs, D = discordant, T_x / T_y = ties in x / y.
kendallTau :: Vector Double -> Vector Double -> SCEResult CorrelationResult
kendallTau xs ys
  | V.length xs < 3       = Left $ mkError E2002
      ("Kendall tau requires >= 3 observations, got "
        <> T.pack (show (V.length xs)))
      ["Collect more data."] Error
  | V.length xs /= V.length ys = Left $ mkError E2001
      ("Vector length mismatch for Kendall tau: xs="
        <> T.pack (show (V.length xs))
        <> ", ys=" <> T.pack (show (V.length ys)))
      ["Both vectors must have the same number of observations."] Error
  | otherwise =
      let pairs  = zip (V.toList xs) (V.toList ys)
          n      = length pairs
          (c, d, tx, ty) = countPairs pairs
          denom  = sqrt (fromIntegral (c + d + tx) * fromIntegral (c + d + ty))
          tau    = if denom < 1e-300 then 0.0
                   else clamp11 (fromIntegral (c - d) / denom)
          -- Approximate p-value via normal approximation for n >= 10
          pval   = if n < 10 then Nothing
                   else
                     let v0  = fromIntegral n * fromIntegral (n - 1) / 2.0
                         vT  = fromIntegral tx * fromIntegral (tx + 1) * fromIntegral (tx + 2) / fromIntegral n
                         vU  = fromIntegral ty * fromIntegral (ty + 1) * fromIntegral (ty + 2) / fromIntegral n
                         varT = (v0 - vT) * (v0 - vU) / (fromIntegral n * fromIntegral (n - 1))
                         z   = if varT < 1e-300 then 0.0
                               else fromIntegral (c - d) / sqrt (2.0 * varT)
                     in Just (2.0 * normSFUpper (abs z))
      in Right $ CorrelationResult
           { corrValue  = tau
           , corrPValue = pval
           , corrCI95Lo = Nothing   -- CI not defined for τ-b
           , corrCI95Hi = Nothing
           , corrN      = n
           }

-- ---------------------------------------------------------------------------
-- Partial correlation
-- ---------------------------------------------------------------------------

-- | First-order (and higher-order) partial correlation of x and y,
-- controlling for a list of covariates zs.
--
-- Uses the recursive formula for first-order partial correlation:
-- @r_xy.z = (r_xy - r_xz · r_yz) / sqrt((1 - r_xz²)(1 - r_yz²))@
--
-- For higher-order: iterates by controlling covariates one at a time.
-- Empty covariate list returns the zero-order Pearson correlation.
partialCorrelation
  :: Vector Double        -- ^ x
  -> Vector Double        -- ^ y
  -> [Vector Double]      -- ^ control variables
  -> SCEResult CorrelationResult
partialCorrelation xs ys []     = pearsonCorrelation xs ys
partialCorrelation xs ys (z:zs) = do
  rxy  <- corrVal <$> pearsonCorrelation xs ys
  rxz  <- corrVal <$> pearsonCorrelation xs z
  ryz  <- corrVal <$> pearsonCorrelation ys z
  let denom = sqrt ((1.0 - rxz * rxz) * (1.0 - ryz * ryz))
  partialR <- if denom < 1e-300
                then Left $ mkError E3001
                  "Partial correlation undefined: control variable is perfectly collinear"
                  ["Remove the collinear control variable."] Error
                else Right $ clamp11 ((rxy - rxz * ryz) / denom)
  -- Synthesise residual vectors and recurse for higher-order partial
  let xRes = residualise xs z
      yRes  = residualise ys z
  if null zs
    then Right $ CorrelationResult
           { corrValue  = partialR
           , corrPValue = fisherPValue partialR (V.length xs)
           , corrCI95Lo = fmap fst (fisherCI95 partialR (V.length xs))
           , corrCI95Hi = fmap snd (fisherCI95 partialR (V.length xs))
           , corrN      = V.length xs
           }
    else do
      hiOrder <- partialCorrelation xRes yRes zs
      Right hiOrder { corrValue = clamp11 (corrValue hiOrder) }
  where
    corrVal = corrValue

-- ---------------------------------------------------------------------------
-- Correlation matrix
-- ---------------------------------------------------------------------------

-- | All pairwise Pearson correlations for n named vectors.
data CorrelationMatrix = CorrelationMatrix
  { cmLabels  :: [T.Text]           -- ^ Column labels in order
  , cmEntries :: Map (T.Text, T.Text) Double  -- ^ (label_i, label_j) → r
  , cmN       :: Int                -- ^ Common sample size
  } deriving stock (Show, Eq)

-- | Build a pairwise Pearson correlation matrix.
-- All vectors must have the same length >= 3.
-- Labels and vectors must have equal list lengths.
correlationMatrix
  :: [(T.Text, Vector Double)]   -- ^ (label, data) pairs
  -> SCEResult CorrelationMatrix
correlationMatrix pairs
  | null pairs = Left $ mkError E2002
      "Correlation matrix requires at least one column"
      ["Provide at least one numeric vector."] Error
  | otherwise =
      let lbls     = map fst pairs
          vecs     = map snd pairs
          -- vecs is non-empty (guarded above); use safe first element
          n0       = case vecs of { (v:_) -> V.length v; [] -> 0 }
      in if any (\v -> V.length v /= n0) vecs
           then Left $ mkError E2001
             "All vectors must have the same length for correlation matrix"
             ["Ensure all columns have the same number of observations."] Error
           else if n0 < 3
             then Left $ mkError E2002
               ("Correlation matrix requires >= 3 observations, got "
                 <> T.pack (show n0))
               ["Collect more data."] Error
             else do
               entries <- buildEntries lbls vecs
               Right CorrelationMatrix
                 { cmLabels  = lbls
                 , cmEntries = entries
                 , cmN       = n0
                 }

-- | Look up a correlation from the matrix (order-independent).
-- Returns Nothing when either label is absent.
lookupCorrelation :: T.Text -> T.Text -> CorrelationMatrix -> Maybe Double
lookupCorrelation l1 l2 cm =
  case M.lookup (l1, l2) (cmEntries cm) of
    Just r  -> Just r
    Nothing -> M.lookup (l2, l1) (cmEntries cm)

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Raw Pearson: two-pass, no length checks (callers guard).
rawPearson :: Vector Double -> Vector Double -> Double
rawPearson xs ys
  | n < 2      = 0.0
  | vx < 1e-300 || vy < 1e-300 = 0.0   -- zero variance → undefined
  | otherwise   = clamp11 $ kahanSum (V.zipWith (\x y -> (x - mx) * (y - my)) xs ys)
                           / sqrt (vx * vy)
  where
    n  = fromIntegral (V.length xs) :: Double
    mx = kahanMean xs
    my = kahanMean ys
    -- Use biased sum-of-squares (Bessel in denominator cancels):
    -- Cov(x,y) / sqrt(Var(x)*Var(y)) -- n-1 cancels out
    vx = kahanSum $ V.map (\x -> (x - mx) * (x - mx)) xs
    vy = kahanSum $ V.map (\y -> (y - my) * (y - my)) ys

-- | Clamp to [-1, 1] to absorb floating-point rounding beyond the boundary.
clamp11 :: Double -> Double
clamp11 x = max (-1.0) (min 1.0 x)

-- | Fractional (mid) ranks — ties assigned the average of their rank positions.
fractionalRanks :: Vector Double -> Vector Double
fractionalRanks v =
  let n       = V.length v
      indexed = V.toList $ V.imap (\i x -> (x, i)) v
      sorted  = sortBy (comparing fst) indexed
      -- Group consecutive tied values
      groups  = groupBy (\a b -> fst a == fst b) sorted
      -- For each group assign average rank (1-based)
      ranked  = concatMap assignRank (zip (scanl (+) 0 (map length groups)) groups)
  in V.fromList $ map snd $ sortBy (comparing fst) ranked
  where
    assignRank :: (Int, [(Double, Int)]) -> [(Int, Double)]
    assignRank (startIdx, grp) =
      let groupSize = length grp
          -- average 1-based rank of the positions in this group
          avgRank   = fromIntegral (2 * startIdx + groupSize + 1) / 2.0
      in [(origIdx, avgRank) | (_, origIdx) <- grp]

-- | Concordant/discordant pair counting for Kendall τ-b.
-- Returns (concordant, discordant, ties_in_x, ties_in_y).
countPairs :: [(Double, Double)] -> (Int, Int, Int, Int)
countPairs pairs = go pairs 0 0 0 0
  where
    go []         c d tx ty = (c, d, tx, ty)
    go ((xi,yi):rest) c d tx ty =
      let (c', d', tx', ty') = foldl (classify xi yi) (c, d, tx, ty) rest
      in go rest c' d' tx' ty'

    classify xi yi (c, d, tx, ty) (xj, yj) =
      let dx = compare xi xj
          dy = compare yi yj
      in case (dx, dy) of
           (EQ, EQ) -> (c, d, tx + 1, ty + 1)
           (EQ, _ ) -> (c, d, tx + 1, ty)
           (_ , EQ) -> (c, d, tx, ty + 1)
           (GT, GT) -> (c + 1, d, tx, ty)
           (LT, LT) -> (c + 1, d, tx, ty)
           _         -> (c, d + 1, tx, ty)

-- | OLS residualise: return x - b*z where b = Cov(x,z)/Var(z).
-- Returns x unchanged when z has zero variance.
residualise :: Vector Double -> Vector Double -> Vector Double
residualise x z
  | V.length x /= V.length z = x   -- length mismatch: return x unchanged
  | otherwise =
      let vz = kahanSum $ V.map (\zi -> (zi - mz) * (zi - mz)) z
          mz = kahanMean z
          mx = kahanMean x
      in if vz < 1e-300
           then x   -- z is constant: no variation to regress out
           else
             let cov_xz = kahanSum $ V.zipWith (\xi zi -> (xi - mx) * (zi - mz)) x z
                 b      = cov_xz / vz
             in V.zipWith (\xi zi -> xi - b * zi) x z

-- | Build all pairwise entries for the correlation matrix.
buildEntries
  :: [T.Text]
  -> [Vector Double]
  -> SCEResult (Map (T.Text, T.Text) Double)
buildEntries lbls vecs = do
  let n = length lbls
  entries <- sequence
    [ do r <- corrValue <$> pearsonCorrelation (vecs !! i) (vecs !! j)
         Right ((lbls !! i, lbls !! j), r)
    | i <- [0 .. n - 1]
    , j <- [0 .. n - 1]
    ]
  Right (M.fromList entries)

-- ---------------------------------------------------------------------------
-- Fisher z-transformation utilities
-- ---------------------------------------------------------------------------

-- | Approximate two-sided p-value via Fisher's z (valid for n >= 10).
fisherPValue :: Double -> Int -> Maybe Double
fisherPValue r n
  | n < 10    = Nothing
  | abs r >= 1.0 = Just 0.0
  | otherwise =
      let z    = fisherZ r
          se   = 1.0 / sqrt (fromIntegral n - 3.0)
          zStat = abs z / se
      in Just (2.0 * normSFUpper zStat)

-- | 95% CI for Pearson r via Fisher z-transformation.
fisherCI95 :: Double -> Int -> Maybe (Double, Double)
fisherCI95 r n
  | n < 10       = Nothing
  | abs r >= 1.0 = Just (r, r)
  | otherwise    =
      let z   = fisherZ r
          se  = 1.0 / sqrt (fromIntegral n - 3.0)
          lo  = z - 1.959964 * se   -- 1.96 ≈ qnorm(0.975)
          hi  = z + 1.959964 * se
      in Just (clamp11 (fisherZInv lo), clamp11 (fisherZInv hi))

-- | Fisher z-transformation: z = atanh(r).
fisherZ :: Double -> Double
fisherZ r = 0.5 * log ((1.0 + r) / (1.0 - r))

-- | Inverse Fisher z: r = tanh(z).
fisherZInv :: Double -> Double
fisherZInv z = (exp (2.0 * z) - 1.0) / (exp (2.0 * z) + 1.0)

-- | Upper tail of standard normal: P(Z > z).
-- Abramowitz & Stegun 26.2.17 rational approximation (max error 7.5e-8).
normSFUpper :: Double -> Double
normSFUpper z
  | z < 0     = 1.0 - normSFUpper (-z)
  | otherwise =
      let t = 1.0 / (1.0 + 0.2316419 * z)
          p = t * (0.319381530
              + t * (-0.356563782
              + t * ( 1.781477937
              + t * (-1.821255978
              + t *   1.330274429))))
          pdf = exp (-0.5 * z * z) / sqrt (2.0 * pi)
      in pdf * p
