{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData         #-}

{-|
Module      : SCE.Ingestion.Schema
Description : Stage 2 of the ingestion pipeline — schema inference and confirmation
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Infers a 'CandidateSchema' from a 'RawFrame' and, once the user (or config)
confirms the intended measurement scales, produces a 'CommittedSchema'.

Key invariants:
  * 'inferSchema' never assigns 'LikelyRatio' to a column with any negative values.
  * 'confirmSchema' returns 'Left' when a user-confirmed scale is contradicted by data.
-}
module SCE.Ingestion.Schema
  ( -- * Schema inference and confirmation
    inferSchema
  , confirmSchema
    -- * Types
  , CandidateSchema(..)
  , ColumnSchema(..)
  , CandidateScale(..)
  , CommittedSchema(..)
  , CommittedColumn(..)
  , SchemaConfirmation(..)
  ) where

import SCE.Core.Types
  ( DetailedError
  , ErrorCode(..)
  , Severity(..)
  , MeasurementScale(..)
  , mkError
  )
import SCE.Ingestion.Parser (RawFrame(..))

import           Data.Text    (Text)
import qualified Data.Text    as T
import           Data.Vector  (Vector)
import qualified Data.Vector  as V
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.List    (nub)
import           Data.Maybe   (mapMaybe)
import           Text.Read    (readMaybe)

------------------------------------------------------------
-- Types
------------------------------------------------------------

-- | Inferred (not yet confirmed) scale — distinct from 'MeasurementScale'.
data CandidateScale
  = LikelyNominal    -- ^ Text values, low unique count relative to total
  | LikelyOrdinal    -- ^ Text values with detectable integer-like rank ordering
  | LikelyInterval   -- ^ Numeric with negative values (no true zero)
  | LikelyRatio      -- ^ Numeric, all non-negative, likely true zero
  | LikelyCount      -- ^ Non-negative integers only, discrete
  | Ambiguous        -- ^ Cannot determine without user input
  deriving stock (Show, Eq)

-- | Per-column statistics and inferred scale.
data ColumnSchema = ColumnSchema
  { csName              :: Text
  , csCandidateScale    :: CandidateScale
  , csNullCount         :: Int
  , csNullPct           :: Double
  , csUniqueCount       :: Int
  , csUniqueRatio       :: Double   -- ^ uniqueCount / totalRows
  , csSampleValues      :: [Text]   -- ^ First 5 non-null values
  , csInferredHaskellType :: Text   -- ^ "Double" | "Int" | "Text"
  , csHasNegatives      :: Bool     -- ^ True if any numeric value < 0
  , csAllNonNegInt      :: Bool     -- ^ True if all parseable values are non-negative Int
  } deriving stock (Show)

-- | Candidate schema for an entire frame.
data CandidateSchema = CandidateSchema
  { schColumns  :: Vector ColumnSchema
  , schRowCount :: Int
  , schWarnings :: [Text]   -- ^ e.g. "Column 'income' has 67% missing values"
  } deriving stock (Show)

-- | User / config confirmation of measurement scales.
data SchemaConfirmation = SchemaConfirmation
  { scConfirmedScales :: Map Text MeasurementScale  -- ^ Explicitly confirmed scales
  , scDropColumns     :: [Text]                      -- ^ Columns to exclude
  , scRenameColumns   :: Map Text Text               -- ^ old name -> new name
  } deriving stock (Show)

-- | A single column with its committed (user-confirmed) scale.
data CommittedColumn = CommittedColumn
  { ccName  :: Text
  , ccScale :: MeasurementScale
  , ccNullPct :: Double
  } deriving stock (Show)

-- | A fully committed schema ready for coercion.
data CommittedSchema = CommittedSchema
  { csCommittedColumns :: Vector CommittedColumn
  , csSource           :: Maybe Text   -- ^ Origin file name
  } deriving stock (Show)

------------------------------------------------------------
-- Schema inference
------------------------------------------------------------
inferAllColumnSchemas :: Vector Text -> Vector (Vector Text) -> Int -> Vector ColumnSchema
inferAllColumnSchemas colNames rows totalRows =
  V.imap (\i colName -> inferColumnSchemaAt i colName rows totalRows) colNames

inferColumnSchemaAt :: Int -> Text -> Vector (Vector Text) -> Int -> ColumnSchema
inferColumnSchemaAt colIdx colName rows totalRows =
  let rawValues   = V.toList $ V.map (\row ->
                      if colIdx < V.length row then row V.! colIdx else "") rows
      nonNull     = filter (not . isMissing) rawValues
      nullCount   = length rawValues - length nonNull
      nullPct     = if totalRows == 0 then 0.0
                    else fromIntegral nullCount / fromIntegral totalRows * 100.0
      uniques     = nub nonNull
      uniqueCount = length uniques
      uniqueRatio = if totalRows == 0 then 0.0
                    else fromIntegral uniqueCount / fromIntegral totalRows
      sample      = take 5 nonNull

      doubles     = mapMaybe (readMaybe . T.unpack) nonNull :: [Double]
      ints        = mapMaybe (readMaybe . T.unpack) nonNull :: [Int]
      allNumeric  = not (null nonNull) && length doubles == length nonNull
      allInt      = not (null nonNull) && length ints == length nonNull
      hasNegs     = any (< 0) doubles
      allNonNegInt = allInt && all (>= 0) ints

      inferredType
        | allInt     = "Int"
        | allNumeric = "Double"
        | otherwise  = "Text"

      candidateScale = inferCandidateScale
                         allNumeric allInt hasNegs allNonNegInt
                         uniqueCount totalRows nonNull

  in ColumnSchema
       { csName              = colName
       , csCandidateScale    = candidateScale
       , csNullCount         = nullCount
       , csNullPct           = nullPct
       , csUniqueCount       = uniqueCount
       , csUniqueRatio       = uniqueRatio
       , csSampleValues      = sample
       , csInferredHaskellType = inferredType
       , csHasNegatives      = hasNegs
       , csAllNonNegInt      = allNonNegInt
       }

-- | Scale inference rules. Never assigns 'LikelyRatio' when negatives exist.
-- Guard order matters: LikelyCount checked before LikelyRatio so that
-- non-negative integers go to Count (discrete) not Ratio (continuous).
inferCandidateScale
  :: Bool   -- ^ all values parseable as Double
  -> Bool   -- ^ all values parseable as Int (unused directly; captured via allNonNegInt)
  -> Bool   -- ^ any value < 0
  -> Bool   -- ^ all non-negative Int
  -> Int    -- ^ unique count
  -> Int    -- ^ total rows
  -> [Text] -- ^ non-null raw values
  -> CandidateScale
inferCandidateScale allNumeric _allInt hasNegs allNonNegInt uniqueCount totalRows nonNull
  | null nonNull           = Ambiguous
  | allNonNegInt           = LikelyCount     -- discrete, non-negative integers
  | allNumeric && hasNegs  = LikelyInterval  -- negative values → no true zero
  | allNumeric             = LikelyRatio     -- non-negative numeric → ratio
  | isLowCardinality       = LikelyNominal   -- few unique text values
  | isOrderedText nonNull  = LikelyOrdinal   -- text that looks ranked
  | otherwise              = LikelyNominal
  where
    isLowCardinality = uniqueCount <= max 2 (totalRows `div` 10)
    isOrderedText vs = all (\v -> T.all (`elem` ("0123456789" :: String)) v && not (T.null v)) vs

isMissing :: Text -> Bool
isMissing t = T.null t || T.toLower t `elem` ["na", "n/a", "null", "none", "nan", ""]

buildWarnings :: Vector ColumnSchema -> [Text]
buildWarnings cols = concatMap warn (V.toList cols)
  where
    warn cs
      | csNullPct cs >= 50.0 =
          ["Column '" <> csName cs <> "' has "
            <> T.pack (show (round (csNullPct cs) :: Int)) <> "% missing values"]
      | csUniqueCount cs == 1 =
          ["Column '" <> csName cs <> "' has only one unique value (constant column)"]
      | otherwise = []

-- | Infer a 'CandidateSchema' from a 'RawFrame'. Pure.
-- Never assigns 'LikelyRatio' to a column with any negative values.
inferSchema :: RawFrame -> Either DetailedError CandidateSchema
inferSchema rf
  | V.null (rfColumns rf) =
      Left $ mkError E1004 "Cannot infer schema: frame has no columns"
               ["Ensure the CSV file has a header row."] Error
  | rfRowCount rf == 0 =
      Left $ mkError E1004 "Cannot infer schema: frame has no rows"
               ["Ensure the CSV file contains data rows."] Error
  | otherwise =
      let colSchemas = inferAllColumnSchemas (rfColumns rf) (rfRows rf) (rfRowCount rf)
          warnings   = buildWarnings colSchemas
      in Right CandidateSchema
           { schColumns  = colSchemas
           , schRowCount = rfRowCount rf
           , schWarnings = warnings
           }

------------------------------------------------------------
-- Schema confirmation
------------------------------------------------------------

-- | Apply a 'SchemaConfirmation' to produce a 'CommittedSchema'.
-- Returns 'Left' if a user-confirmed scale is contradicted by the data.
confirmSchema
  :: CandidateSchema
  -> SchemaConfirmation
  -> Either DetailedError CommittedSchema
confirmSchema candidate confirmation = do
  let dropped  = scDropColumns confirmation
      renamed  = scRenameColumns confirmation
      confirmed = scConfirmedScales confirmation

  -- Filter dropped columns
  let kept = V.filter (\cs -> csName cs `notElem` dropped) (schColumns candidate)

  -- Validate each confirmed scale against inferred data properties
  committed <- V.mapM (commitColumn confirmed renamed) kept

  if V.null committed
    then Left $ mkError E1004 "All columns were dropped; no schema remains"
                  ["Keep at least one column."] Error
    else Right CommittedSchema
           { csCommittedColumns = committed
           , csSource           = Nothing
           }

commitColumn
  :: Map Text MeasurementScale
  -> Map Text Text
  -> ColumnSchema
  -> Either DetailedError CommittedColumn
commitColumn confirmed renamed cs = do
  let name      = csName cs
      finalName = M.findWithDefault name name renamed
      scale     = case M.lookup name confirmed of
                    Just s  -> s
                    Nothing -> candidateToScale (csCandidateScale cs)

  -- Validate confirmed scale against actual data
  case M.lookup name confirmed of
    Nothing -> Right ()  -- inferred — always valid
    Just Ratio ->
      if csHasNegatives cs
        then Left $ mkError E2003
               ("Column '" <> name <> "' was confirmed as Ratio scale but contains negative values")
               ["Use Interval scale for columns with negative values."] Error
        else Right ()
    Just Interval ->
      if csInferredHaskellType cs == "Text" && not (csHasNegatives cs)
        then Left $ mkError E2003
               ("Column '" <> name <> "' was confirmed as Interval but appears to contain text")
               ["Use Nominal or Ordinal scale for text columns."] Error
        else Right ()
    Just _ -> Right ()  -- Nominal/Ordinal always compatible

  Right CommittedColumn
    { ccName    = finalName
    , ccScale   = scale
    , ccNullPct = csNullPct cs
    }

-- | Map candidate scale to committed 'MeasurementScale'.
candidateToScale :: CandidateScale -> MeasurementScale
candidateToScale LikelyNominal  = Nominal
candidateToScale LikelyOrdinal  = Ordinal
candidateToScale LikelyInterval = Interval
candidateToScale LikelyRatio    = Ratio
candidateToScale LikelyCount    = Ratio   -- counts are Ratio scale
candidateToScale Ambiguous      = Nominal  -- safe default
