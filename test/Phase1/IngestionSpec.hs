{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}

module Phase1.IngestionSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import SCE.Ingestion.Parser
import SCE.Ingestion.Schema
import SCE.Ingestion.MissingData
import SCE.Ingestion.Coercion
import SCE.Core.Types  (MeasurementScale(..), ErrorCode(..))
import SCE.Core.Error  (errorCode, DetailedError)

import qualified Data.Vector     as V
import qualified Data.Map.Strict as M
import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Data.ByteString.Lazy.Char8 as BLC

------------------------------------------------------------
-- Helpers
------------------------------------------------------------

csvBytes :: [String] -> BLC.ByteString
csvBytes = BLC.pack . unlines

makeRawFrame :: [Text] -> [[Text]] -> RawFrame
makeRawFrame cols rows = RawFrame
  { rfColumns  = V.fromList cols
  , rfRows     = V.fromList (map V.fromList rows)
  , rfSource   = Nothing
  , rfRowCount = length rows
  , rfColCount = length cols
  }

defaultConf :: SchemaConfirmation
defaultConf = SchemaConfirmation
  { scConfirmedScales = M.empty
  , scDropColumns     = []
  , scRenameColumns   = M.empty
  }

showErr :: DetailedError -> String
showErr e = "[" ++ show (errorCode e) ++ "] error (see formatError for full details)"

------------------------------------------------------------
-- Spec
------------------------------------------------------------

spec :: Spec
spec = do

  describe "SCE.Ingestion.Parser" $ do

    describe "detectDelimiter" $ do
      it "detects comma" $
        detectDelimiter (csvBytes ["a,b,c","1,2,3","4,5,6"])
          `shouldBe` Right ','
      it "detects tab" $
        detectDelimiter (csvBytes ["a\tb\tc","1\t2\t3","4\t5\t6"])
          `shouldBe` Right '\t'
      it "detects semicolon" $
        detectDelimiter (csvBytes ["a;b;c","1;2;3","4;5;6"])
          `shouldBe` Right ';'
      it "detects pipe" $
        detectDelimiter (csvBytes ["a|b|c","1|2|3","4|5|6"])
          `shouldBe` Right '|'
      it "returns Left on empty input" $
        detectDelimiter "" `shouldSatisfy` \case
          Left _  -> True
          Right _ -> False

    describe "parseCSVBytes" $ do
      it "parses simple comma CSV" $ do
        let result = parseCSVBytes defaultParseConfig
                       (csvBytes ["name,value","alice,42","bob,99"])
        case result of
          Left e  -> expectationFailure (showErr e)
          Right rf -> do
            V.length (rfColumns rf) `shouldBe` 2
            rfRowCount rf `shouldBe` 2
            rfColCount rf `shouldBe` 2

      it "auto-detects tab delimiter" $ do
        case parseCSVBytes defaultParseConfig (csvBytes ["x\ty","1\t2","3\t4"]) of
          Left e  -> expectationFailure (showErr e)
          Right rf -> rfColCount rf `shouldBe` 2

      it "respects pcMaxRows" $ do
        let cfg = defaultParseConfig { pcMaxRows = Just 2 }
        case parseCSVBytes cfg (csvBytes ["a,b","1,2","3,4","5,6","7,8"]) of
          Left e  -> expectationFailure (showErr e)
          Right rf -> rfRowCount rf `shouldBe` 2

      it "skips comment lines" $ do
        let cfg = defaultParseConfig { pcCommentChar = Just '#' }
        case parseCSVBytes cfg (csvBytes ["a,b","# comment","1,2","3,4"]) of
          Left e  -> expectationFailure (showErr e)
          Right rf -> rfRowCount rf `shouldBe` 2

      it "returns Left on empty input" $
        parseCSVBytes defaultParseConfig { pcDelimiter = Just ',' } ""
          `shouldSatisfy` \case
            Left _  -> True
            Right _ -> False

      it "preserves raw text values exactly" $ do
        case parseCSVBytes defaultParseConfig (csvBytes ["v","hello world","42",""]) of
          Left e  -> expectationFailure (showErr e)
          Right rf ->
            V.toList (rfRows rf V.! 0) `shouldBe` ["hello world"]

  describe "SCE.Ingestion.Schema" $ do

    describe "inferSchema" $ do
      it "Left E1004 for no-column frame" $
        inferSchema (makeRawFrame [] []) `shouldSatisfy` \case
          Left e  -> errorCode e == E1004
          Right _ -> False

      it "Left E1004 for zero-row frame" $
        inferSchema (makeRawFrame ["x"] []) `shouldSatisfy` \case
          Left e  -> errorCode e == E1004
          Right _ -> False

      it "LikelyRatio for all-positive numeric" $ do
        case inferSchema (makeRawFrame ["p"] [["10.5"],["20.0"],["0.0"],["5.1"]]) of
          Left e  -> expectationFailure (showErr e)
          Right cs ->
            csCandidateScale (V.head (schColumns cs)) `shouldBe` LikelyRatio

      it "LikelyInterval for column with negatives" $ do
        case inferSchema (makeRawFrame ["t"] [["-5.0"],["0.0"],["10.0"]]) of
          Left e  -> expectationFailure (showErr e)
          Right cs ->
            csCandidateScale (V.head (schColumns cs)) `shouldBe` LikelyInterval

      it "NEVER LikelyRatio when any value is negative" $ do
        case inferSchema (makeRawFrame ["x"] [["-0.001"],["2.0"],["3.0"]]) of
          Left e  -> expectationFailure (showErr e)
          Right cs ->
            csCandidateScale (V.head (schColumns cs)) `shouldNotBe` LikelyRatio

      it "LikelyCount for non-negative integers" $ do
        case inferSchema (makeRawFrame ["n"] [["0"],["5"],["10"],["3"]]) of
          Left e  -> expectationFailure (showErr e)
          Right cs ->
            csCandidateScale (V.head (schColumns cs)) `shouldBe` LikelyCount

      it "LikelyNominal for low-cardinality text" $ do
        let rows = map (:[]) (concat (replicate 4 ["A","B","C"]))
        case inferSchema (makeRawFrame ["c"] rows) of
          Left e  -> expectationFailure (showErr e)
          Right cs ->
            csCandidateScale (V.head (schColumns cs)) `shouldBe` LikelyNominal

      it "counts nulls and pct correctly" $ do
        case inferSchema (makeRawFrame ["x"] [[""],[""],["1.0"],["2.0"]]) of
          Left e  -> expectationFailure (showErr e)
          Right cs -> do
            csNullCount (V.head (schColumns cs)) `shouldBe` 2
            csNullPct   (V.head (schColumns cs)) `shouldBe` 50.0

      it "treats na/null/NA as missing" $ do
        case inferSchema (makeRawFrame ["x"] [["na"],["NA"],["null"],["1.0"]]) of
          Left e  -> expectationFailure (showErr e)
          Right cs ->
            csNullCount (V.head (schColumns cs)) `shouldBe` 3

      it "warns on >=50% missing" $ do
        let rows = map (:[]) (replicate 6 "" ++ replicate 4 "1.0")
        case inferSchema (makeRawFrame ["x"] rows) of
          Left e  -> expectationFailure (showErr e)
          Right cs -> schWarnings cs `shouldSatisfy` (not . null)

      it "warns on constant column" $ do
        case inferSchema (makeRawFrame ["x"] [["42"],["42"],["42"]]) of
          Left e  -> expectationFailure (showErr e)
          Right cs -> schWarnings cs `shouldSatisfy` (not . null)

      it "reports correct unique count" $ do
        case inferSchema (makeRawFrame ["x"] [["A"],["B"],["A"],["C"]]) of
          Left e  -> expectationFailure (showErr e)
          Right cs ->
            csUniqueCount (V.head (schColumns cs)) `shouldBe` 3

    describe "confirmSchema" $ do
      it "rejects Ratio for negative column (E2003)" $ do
        case inferSchema (makeRawFrame ["x"] [["-1.0"],["2.0"],["3.0"]]) of
          Left e  -> expectationFailure (showErr e)
          Right cs -> do
            let conf = defaultConf { scConfirmedScales = M.fromList [("x", Ratio)] }
            confirmSchema cs conf `shouldSatisfy` \case
              Left e  -> errorCode e == E2003
              Right _ -> False

      it "accepts Interval for negative column" $ do
        case inferSchema (makeRawFrame ["t"] [["-5.0"],["10.0"]]) of
          Left e  -> expectationFailure (showErr e)
          Right cs -> do
            let conf = defaultConf { scConfirmedScales = M.fromList [("t", Interval)] }
            confirmSchema cs conf `shouldSatisfy` \case
              Right _ -> True
              Left _  -> False

      it "drops specified columns" $ do
        case inferSchema (makeRawFrame ["a","b"] [["1","x"],["2","y"]]) of
          Left e  -> expectationFailure (showErr e)
          Right cs -> do
            let conf = defaultConf { scDropColumns = ["b"] }
            case confirmSchema cs conf of
              Left e  -> expectationFailure (showErr e)
              Right sc -> V.length (csCommittedColumns sc) `shouldBe` 1

      it "Left E1004 when all columns dropped" $ do
        case inferSchema (makeRawFrame ["a"] [["1"],["2"]]) of
          Left e  -> expectationFailure (showErr e)
          Right cs -> do
            let conf = defaultConf { scDropColumns = ["a"] }
            confirmSchema cs conf `shouldSatisfy` \case
              Left e  -> errorCode e == E1004
              Right _ -> False

      it "renames columns" $ do
        case inferSchema (makeRawFrame ["old"] [["1.0"],["2.0"]]) of
          Left e  -> expectationFailure (showErr e)
          Right cs -> do
            let conf = defaultConf { scRenameColumns = M.fromList [("old","new")] }
            case confirmSchema cs conf of
              Left e  -> expectationFailure (showErr e)
              Right sc ->
                ccName (V.head (csCommittedColumns sc)) `shouldBe` "new"

  describe "SCE.Ingestion.MissingData" $ do

    describe "analyseMissingness" $ do
      it "no missing values -> CompleteData for all columns" $ do
        let r = analyseMissingness
                  (makeRawFrame ["x","y"] [["1","2"],["3","4"],["5","6"]])
        mrAnyMissingRows r `shouldBe` 0
        mrCompleteRows   r `shouldBe` 3
        all (\c -> cmPattern c == CompleteData) (mrColumns r) `shouldBe` True

      it "100% missing column -> MNAR" $ do
        let r = analyseMissingness (makeRawFrame ["x"] [[""],[""],[""],[""]])
        case mrColumns r of
          [cm] -> do
            cmMissingN   cm `shouldBe` 4
            cmMissingPct cm `shouldBe` 100.0
            cmPattern    cm `shouldBe` MNAR
          other -> expectationFailure ("Expected 1 column, got " ++ show (length other))

      it "partial missing -> correct complete/anyMissing counts" $ do
        let r = analyseMissingness
                  (makeRawFrame ["a","b"] [["1","2"],["","2"],["3",""],["4","5"]])
        mrCompleteRows   r `shouldBe` 2
        mrAnyMissingRows r `shouldBe` 2

      it "caps cmRows at 20" $ do
        let r = analyseMissingness (makeRawFrame ["x"] (replicate 30 [""]))
        case mrColumns r of
          [cm] -> length (cmRows cm) `shouldBe` 20
          _    -> expectationFailure "Expected 1 column"

    describe "completeCaseFilter" $ do
      it "removes rows with any missing value in target columns" $ do
        case completeCaseFilter ["x","y"]
               (makeRawFrame ["x","y"] [["1","2"],["","2"],["3","4"]]) of
          Left e  -> expectationFailure (showErr e)
          Right f -> rfRowCount f `shouldBe` 2

      it "Left E2002 when all rows have missing" $
        completeCaseFilter ["x"] (makeRawFrame ["x"] [[""],[""],[]]) `shouldSatisfy` \case
          Left e  -> errorCode e == E2002
          Right _ -> False

      it "empty target list checks all columns" $ do
        case completeCaseFilter []
               (makeRawFrame ["a","b"] [["1","2"],["","3"],["4","5"]]) of
          Left e  -> expectationFailure (showErr e)
          Right f -> rfRowCount f `shouldBe` 2

  describe "SCE.Ingestion.Coercion" $ do

    let mkSchema cols = CommittedSchema (V.fromList cols) Nothing

    describe "coerceFrame" $ do
      it "NumericCol for Ratio column" $ do
        let rf     = makeRawFrame ["v"] [["1.0"],["2.0"],["3.0"]]
            schema = mkSchema [CommittedColumn "v" Ratio 0.0]
        case coerceFrame schema (analyseMissingness rf) rf of
          Left e  -> expectationFailure (showErr e)
          Right vf -> case V.head (vfColumns vf) of
            NumericCol _ Ratio _ -> pure ()
            c -> expectationFailure ("Expected NumericCol Ratio, got " ++ show c)

      it "NumericCol for Interval column" $ do
        let rf     = makeRawFrame ["t"] [["-1.0"],["0.0"],["5.0"]]
            schema = mkSchema [CommittedColumn "t" Interval 0.0]
        case coerceFrame schema (analyseMissingness rf) rf of
          Left e  -> expectationFailure (showErr e)
          Right vf -> case V.head (vfColumns vf) of
            NumericCol _ Interval _ -> pure ()
            c -> expectationFailure ("Expected NumericCol Interval, got " ++ show c)

      it "CategoricCol for Nominal column" $ do
        let rf     = makeRawFrame ["c"] [["A"],["B"],["A"]]
            schema = mkSchema [CommittedColumn "c" Nominal 0.0]
        case coerceFrame schema (analyseMissingness rf) rf of
          Left e  -> expectationFailure (showErr e)
          Right vf -> case V.head (vfColumns vf) of
            CategoricCol _ Nominal _ -> pure ()
            c -> expectationFailure ("Expected CategoricCol Nominal, got " ++ show c)

      it "column count equals schema column count" $ do
        let rf = makeRawFrame ["a","b","c"]
                   [["1.0","X","5"],["2.0","Y","6"]]
            schema = mkSchema
              [ CommittedColumn "a" Ratio   0.0
              , CommittedColumn "b" Nominal 0.0
              , CommittedColumn "c" Ratio   0.0 ]
        case coerceFrame schema (analyseMissingness rf) rf of
          Left e  -> expectationFailure (showErr e)
          Right vf -> V.length (vfColumns vf) `shouldBe` 3

      it "Left E2002 for zero-row frame" $ do
        let rf = makeRawFrame ["x"] []
        coerceFrame (mkSchema [CommittedColumn "x" Ratio 0.0])
                    (analyseMissingness rf) rf
          `shouldSatisfy` \case
            Left e  -> errorCode e == E2002
            Right _ -> False

      it "Left E1004 for empty schema" $ do
        let rf = makeRawFrame ["x"] [["1.0"]]
        coerceFrame (mkSchema []) (analyseMissingness rf) rf
          `shouldSatisfy` \case
            Left e  -> errorCode e == E1004
            Right _ -> False

      it "Left E2004 when schema column not in frame" $ do
        let rf = makeRawFrame ["x"] [["1.0"]]
        coerceFrame (mkSchema [CommittedColumn "nope" Ratio 0.0])
                    (analyseMissingness rf) rf
          `shouldSatisfy` \case
            Left e  -> errorCode e == E2004
            Right _ -> False

      it "produces CoercionWarning for unparseable numeric values" $ do
        let rf     = makeRawFrame ["x"] [["1.0"],["bad"],["3.0"]]
            schema = mkSchema [CommittedColumn "x" Ratio 0.0]
        case coerceFrame schema (analyseMissingness rf) rf of
          Left e  -> expectationFailure (showErr e)
          Right vf -> vfWarnings vf `shouldSatisfy` (not . null)

      it "strips missing values from categoric column" $ do
        let rf     = makeRawFrame ["c"] [["A"],[""],["B"],["NA"]]
            schema = mkSchema [CommittedColumn "c" Nominal 0.0]
        case coerceFrame schema (analyseMissingness rf) rf of
          Left e  -> expectationFailure (showErr e)
          Right vf -> case V.head (vfColumns vf) of
            CategoricCol _ _ v -> V.length v `shouldBe` 2
            c -> expectationFailure ("Unexpected: " ++ show c)

    describe "typed column accessors" $ do
      it "getNumericVec Just for NumericCol" $
        getNumericVec (NumericCol "x" Ratio (V.fromList [1.0,2.0]))
          `shouldBe` Just (V.fromList [1.0,2.0])
      it "getNumericVec promotes IntegerCol" $
        getNumericVec (IntegerCol "x" Ratio (V.fromList [1,2]))
          `shouldBe` Just (V.fromList [1.0,2.0])
      it "getNumericVec Nothing for CategoricCol" $
        getNumericVec (CategoricCol "x" Nominal (V.fromList ["A"]))
          `shouldBe` Nothing
      it "getCategoricVec Just for CategoricCol" $
        getCategoricVec (CategoricCol "x" Nominal (V.fromList ["A","B"]))
          `shouldBe` Just (V.fromList ["A","B"])
      it "columnName works for all constructors" $ do
        columnName (NumericCol   "a" Ratio   V.empty) `shouldBe` "a"
        columnName (IntegerCol   "b" Ratio   V.empty) `shouldBe` "b"
        columnName (CategoricCol "c" Nominal V.empty) `shouldBe` "c"

  describe "QuickCheck properties" $ do

    it "prop: coerceFrame column count = schema column count" $
      property $ \(Positive (n :: Int)) ->
        let nCols  = (n `mod` 5) + 1
            names  = [T.pack ("c" ++ show i) | i <- [1..nCols]]
            rf     = makeRawFrame names [map (const "1.0") names]
            schema = CommittedSchema
                       (V.fromList [CommittedColumn (T.pack ("c"++show i)) Ratio 0.0
                                   | i <- [1..nCols]])
                       Nothing
        in case coerceFrame schema (analyseMissingness rf) rf of
             Right vf -> V.length (vfColumns vf) == nCols
             Left  _  -> False

    it "prop: inferSchema never assigns LikelyRatio to column with negatives" $
      property $ \(xs :: [Int]) ->
        let negVals = [T.pack (show (negate (abs x) - 1)) | x <- take 4 xs]
            rows    = map (:[]) negVals ++ [["1.0"]]  -- at least one pos to have data
        in not (null negVals) ==>
             case inferSchema (makeRawFrame ["x"] rows) of
               Left  _ -> True
               Right cs ->
                 csCandidateScale (V.head (schColumns cs)) /= LikelyRatio

    it "prop: completeCaseFilter never increases row count" $
      property $ \(Positive (n :: Int)) ->
        let nRows = (n `mod` 20) + 1
            rf    = makeRawFrame ["x"] (replicate nRows ["1.0"])
        in case completeCaseFilter ["x"] rf of
             Left  _  -> True
             Right rf' -> rfRowCount rf' <= nRows

    it "prop: parseCSVBytes row count = number of data rows" $
      property $ \(Positive (n :: Int)) ->
        let nRows    = (n `mod` 20) + 1
            dataRows = [show i ++ "," ++ show (i*2) | i <- [1..nRows]]
            bs       = csvBytes ("a,b" : dataRows)
        in case parseCSVBytes defaultParseConfig bs of
             Left  _ -> False
             Right rf -> rfRowCount rf == nRows

    it "prop: mrCompleteRows + mrAnyMissingRows = rfRowCount" $
      property $ \(Positive (n :: Int)) ->
        let nRows  = (n `mod` 20) + 1
            rf     = makeRawFrame ["x"] (replicate nRows ["1.0"])
            report = analyseMissingness rf
        in mrCompleteRows report + mrAnyMissingRows report == nRows
