{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module      : Phase0.ErrorSpec
Description : Test suite for SCE.Core.Error (Phase 0)
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Covers the Phase 0 acceptance criteria:
  * Every ErrorCode has at least one construction test
  * formatError produces valid ASCII (no non-ASCII characters)
  * formatError is always non-empty
  * formatErrorShort is always non-empty
  * addContext / addSuggestion helpers work correctly
  * Severity ordering is correct
  * StatEngineError mapping is exhaustive
-}
module Phase0.ErrorSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Text as T
import Data.Char (isAscii)

import SCE.Core.Error

-- ---------------------------------------------------------------------------
-- Orphan Arbitrary instances for QuickCheck
-- ---------------------------------------------------------------------------

instance Arbitrary ErrorCode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Severity where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ErrorContext where
  arbitrary = oneof
    [ pure NoContext
    , FileContext <$> arbitraryFilePath <*> positiveInt <*> positiveInt
    , DataContext <$> arbitraryColName <*> positiveInt <*> arbitraryText
    , ComputationContext <$> arbitraryOpName <*> smallList (liftA2 (,) arbitraryText arbitraryText)
    ]
    where
      positiveInt  = getPositive <$> arbitrary
      arbitraryFilePath  = elements ["/data/sample.csv", "data.csv", "out/results.csv"]
      arbitraryColName   = elements ["age", "income", "score", "category"]
      arbitraryOpName    = elements ["t-test", "ANOVA", "mean", "variance"]
      arbitraryText      = elements ["42", "hello", "3.14", "NA", ""]
      smallList gen      = listOf gen `suchThat` (\l -> length l <= 5)

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "ErrorCode enum" $ do
    it "covers all codes from E1001 to E5003" $
      length [minBound .. maxBound :: ErrorCode] `shouldBe` 24

    it "every code has a non-empty prefix" $
      all (not . T.null . errorCodePrefix) [minBound .. maxBound :: ErrorCode]
        `shouldBe` True

    it "every prefix starts with '[E'" $
      all (T.isPrefixOf "[E" . errorCodePrefix) [minBound .. maxBound :: ErrorCode]
        `shouldBe` True

  -- -------------------------------------------------------------------------
  -- Construction: one test per ErrorCode
  -- -------------------------------------------------------------------------
  describe "mkError construction" $ do
    let mkE code = mkError code "test" [] Error

    it "E1001 builds without exception" $ errorCode (mkE E1001) `shouldBe` E1001
    it "E1002 builds without exception" $ errorCode (mkE E1002) `shouldBe` E1002
    it "E1003 builds without exception" $ errorCode (mkE E1003) `shouldBe` E1003
    it "E1004 builds without exception" $ errorCode (mkE E1004) `shouldBe` E1004
    it "E1005 builds without exception" $ errorCode (mkE E1005) `shouldBe` E1005
    it "E2001 builds without exception" $ errorCode (mkE E2001) `shouldBe` E2001
    it "E2002 builds without exception" $ errorCode (mkE E2002) `shouldBe` E2002
    it "E2003 builds without exception" $ errorCode (mkE E2003) `shouldBe` E2003
    it "E2004 builds without exception" $ errorCode (mkE E2004) `shouldBe` E2004
    it "E2005 builds without exception" $ errorCode (mkE E2005) `shouldBe` E2005
    it "E2006 builds without exception" $ errorCode (mkE E2006) `shouldBe` E2006
    it "E2007 builds without exception" $ errorCode (mkE E2007) `shouldBe` E2007
    it "E2008 builds without exception" $ errorCode (mkE E2008) `shouldBe` E2008
    it "E3001 builds without exception" $ errorCode (mkE E3001) `shouldBe` E3001
    it "E3002 builds without exception" $ errorCode (mkE E3002) `shouldBe` E3002
    it "E3003 builds without exception" $ errorCode (mkE E3003) `shouldBe` E3003
    it "E3004 builds without exception" $ errorCode (mkE E3004) `shouldBe` E3004
    it "E4001 builds without exception" $ errorCode (mkE E4001) `shouldBe` E4001
    it "E4002 builds without exception" $ errorCode (mkE E4002) `shouldBe` E4002
    it "E4003 builds without exception" $ errorCode (mkE E4003) `shouldBe` E4003
    it "E4004 builds without exception" $ errorCode (mkE E4004) `shouldBe` E4004
    it "E5001 builds without exception" $ errorCode (mkE E5001) `shouldBe` E5001
    it "E5002 builds without exception" $ errorCode (mkE E5002) `shouldBe` E5002
    it "E5003 builds without exception" $ errorCode (mkE E5003) `shouldBe` E5003

  -- -------------------------------------------------------------------------
  -- Severity ordering
  -- -------------------------------------------------------------------------
  describe "Severity ordering" $ do
    it "Info < Warning" $ Info < Warning `shouldBe` True
    it "Warning < Error" $ Warning < Error `shouldBe` True
    it "Error < Critical" $ Error < Critical `shouldBe` True
    it "minBound is Info"     $ (minBound :: Severity) `shouldBe` Info
    it "maxBound is Critical" $ (maxBound :: Severity) `shouldBe` Critical

  -- -------------------------------------------------------------------------
  -- Context helpers
  -- -------------------------------------------------------------------------
  describe "addContext" $ do
    it "replaces NoContext with FileContext" $ do
      let e   = mkError E1001 "test" [] Error
          ctx = FileContext "/a.csv" 3 7
          e'  = addContext ctx e
      errorContext e' `shouldBe` ctx

    it "replaces DataContext with ComputationContext" $ do
      let e   = addContext (DataContext "col" 0 "val") (mkError E2002 "test" [] Error)
          ctx = ComputationContext "mean" [("n","5")]
          e'  = addContext ctx e
      errorContext e' `shouldBe` ctx

  describe "addSuggestion" $ do
    it "appends to empty suggestion list" $ do
      let e  = mkError E2003 "test" [] Warning
          e' = addSuggestion "Try Ratio scale." e
      errorSuggestions e' `shouldBe` ["Try Ratio scale."]

    it "appends to non-empty suggestion list" $ do
      let e  = mkError E2003 "test" ["First."] Warning
          e' = addSuggestion "Second." e
      length (errorSuggestions e') `shouldBe` 2
      last (errorSuggestions e')   `shouldBe` "Second."

  -- -------------------------------------------------------------------------
  -- mkErrorAt
  -- -------------------------------------------------------------------------
  describe "mkErrorAt" $ do
    it "sets context directly" $ do
      let ctx = ComputationContext "variance" [("n","10")]
          e   = mkErrorAt E3001 "numerical instability" ctx ["Use more data."] Error
      errorContext e   `shouldBe` ctx
      errorCode    e   `shouldBe` E3001
      errorSeverity e  `shouldBe` Error

  -- -------------------------------------------------------------------------
  -- StatEngineError category mapping
  -- -------------------------------------------------------------------------
  describe "StatEngineError category" $ do
    it "E1xxx codes map to IngestionError" $
      all (\code -> errorCategory (mkError code "x" [] Error) == IngestionError)
          [E1001, E1002, E1003, E1004, E1005]
        `shouldBe` True

    it "E2xxx codes map to ValidationError" $
      all (\code -> errorCategory (mkError code "x" [] Error) == ValidationError)
          [E2001, E2002, E2003, E2004, E2005, E2006, E2007, E2008]
        `shouldBe` True

    it "E3xxx codes map to StatisticalError" $
      all (\code -> errorCategory (mkError code "x" [] Error) == StatisticalError)
          [E3001, E3002, E3003, E3004]
        `shouldBe` True

    it "E4xxx codes map to InferenceError" $
      all (\code -> errorCategory (mkError code "x" [] Error) == InferenceError)
          [E4001, E4002, E4003, E4004]
        `shouldBe` True

    it "E5xxx codes map to RenderingError" $
      all (\code -> errorCategory (mkError code "x" [] Error) == RenderingError)
          [E5001, E5002, E5003]
        `shouldBe` True

  -- -------------------------------------------------------------------------
  -- formatError — ASCII-only, non-empty
  -- -------------------------------------------------------------------------
  describe "formatError" $ do
    it "is non-empty for a basic error" $
      (not . T.null) (formatError (mkError E2002 "Insufficient data" [] Error))
        `shouldBe` True

    it "contains the error code" $ do
      let e = mkError E2002 "Insufficient data" [] Error
      T.isInfixOf "E2002" (formatError e) `shouldBe` True

    it "contains the message" $ do
      let e = mkError E2002 "Insufficient data" [] Error
      T.isInfixOf "Insufficient data" (formatError e) `shouldBe` True

    it "contains suggestion when present" $ do
      let e = mkError E2002 "Not enough points" ["Collect 30 samples."] Error
      T.isInfixOf "Collect 30 samples." (formatError e) `shouldBe` True

    it "contains FileContext fields" $ do
      let e = mkErrorAt E1001 "parse fail"
                (FileContext "/data/a.csv" 5 12) [] Error
      T.isInfixOf "/data/a.csv" (formatError e) `shouldBe` True
      T.isInfixOf "5"           (formatError e) `shouldBe` True

    it "contains DataContext fields" $ do
      let e = mkErrorAt E2001 "bad value"
                (DataContext "income" 42 "negative") [] Error
      T.isInfixOf "income"   (formatError e) `shouldBe` True
      T.isInfixOf "42"       (formatError e) `shouldBe` True

    it "contains ComputationContext operation name" $ do
      let e = mkErrorAt E3001 "unstable"
                (ComputationContext "t-test" [("n","5")]) [] Error
      T.isInfixOf "t-test" (formatError e) `shouldBe` True

    it "is ASCII-only" $ do
      let e = mkErrorAt E2002 "Insufficient data"
                (FileContext "/data/a.csv" 1 1)
                ["Collect more samples."] Error
      T.all isAscii (formatError e) `shouldBe` True

  -- -------------------------------------------------------------------------
  -- formatErrorShort
  -- -------------------------------------------------------------------------
  describe "formatErrorShort" $ do
    it "is non-empty" $
      (not . T.null) (formatErrorShort (mkError E2002 "Insufficient data" [] Error))
        `shouldBe` True

    it "is a single line (no newlines)" $ do
      let e = mkError E2002 "Insufficient data" ["Get more."] Warning
      T.count "\n" (formatErrorShort e) `shouldBe` 0

    it "contains error code and message" $ do
      let e = mkError E4001 "Normality assumption violated" [] Error
      let s = formatErrorShort e
      T.isInfixOf "E4001" s `shouldBe` True
      T.isInfixOf "Normality assumption violated" s `shouldBe` True

  -- -------------------------------------------------------------------------
  -- severityLabel
  -- -------------------------------------------------------------------------
  describe "severityLabel" $ do
    it "Info    -> INFO"     $ severityLabel Info     `shouldBe` "INFO"
    it "Warning -> WARN"     $ severityLabel Warning  `shouldBe` "WARN"
    it "Error   -> ERROR"    $ severityLabel Error    `shouldBe` "ERROR"
    it "Critical -> CRITICAL" $ severityLabel Critical `shouldBe` "CRITICAL"

  -- -------------------------------------------------------------------------
  -- QuickCheck properties
  -- -------------------------------------------------------------------------
  describe "QuickCheck properties" $ do
    it "prop: formatError is always non-empty" $
      property $ \code sev ctx ->
        let e = mkErrorAt code "msg" ctx [] sev
        in not (T.null (formatError e))

    it "prop: formatErrorShort is always non-empty" $
      property $ \code sev ->
        let e = mkError code "msg" [] sev
        in not (T.null (formatErrorShort e))

    it "prop: formatError output is all ASCII" $
      property $ \code sev ->
        let e = mkError code "test error message" [] sev
        in T.all isAscii (formatError e)

    it "prop: addContext always updates errorContext" $
      property $ \code sev ->
        let e   = mkError code "msg" [] sev
            ctx = FileContext "/f.csv" 1 1
            e'  = addContext ctx e
        in errorContext e' == ctx

    it "prop: addSuggestion monotonically grows suggestion list" $
      property $ \code sev ->
        let e  = mkError code "msg" [] sev
            e' = addSuggestion "sugg" e
        in length (errorSuggestions e') == length (errorSuggestions e) + 1
