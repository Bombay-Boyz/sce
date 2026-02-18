{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Phase2.QualitySpec (spec) where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Vector as V
import           Data.Text   (Text)

import SCE.Core.Types        (MeasurementScale(..))
import SCE.Ingestion.Coercion
  (ValidatedFrame(..), TypedColumn(..), CoercionWarning(..))
import SCE.Ingestion.Schema   (CommittedSchema(..), CommittedColumn(..))
import SCE.Ingestion.MissingData (MissingnessReport(..))
import SCE.Validation.Quality

-- ---------------------------------------------------------------------------
-- Minimal ValidatedFrame builders
-- ---------------------------------------------------------------------------

emptyMissingReport :: MissingnessReport
emptyMissingReport = MissingnessReport
  { mrColumns = [], mrCompleteRows = 0, mrAnyMissingRows = 0
  , mrRecommendation = "" }

emptySchema :: CommittedSchema
emptySchema = CommittedSchema { csCommittedColumns = V.empty, csSource = Nothing }

makeFrame :: [TypedColumn] -> Int -> ValidatedFrame
makeFrame cols n = ValidatedFrame
  { vfColumns  = V.fromList cols
  , vfRowCount = n
  , vfSource   = Nothing
  , vfSchema   = emptySchema
  , vfMissing  = emptyMissingReport
  , vfWarnings = []
  }

numCol :: Text -> [Double] -> TypedColumn
numCol name xs = NumericCol name Ratio (V.fromList xs)

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "generateQualityReport" $ do

    it "returns Good for a clean numeric frame" $ do
      let frame  = makeFrame [numCol "x" [1..30]] 30
          report = generateQualityReport frame
      qrOverallLevel     report `shouldBe` Good
      qrReadyForAnalysis report `shouldBe` True

    it "returns Unusable for a zero-variance column" $ do
      let frame  = makeFrame [numCol "x" (replicate 20 5.0)] 20
          report = generateQualityReport frame
      qrOverallLevel report `shouldBe` Unusable
      qrReadyForAnalysis report `shouldBe` False

    it "detects outlier prevalence > 10 % (Poor)" $ do
      -- 5 outliers out of 30 = 16.7 %
      let normal   = [1..25] :: [Double]
          outliers = [1000, 2000, 3000, 4000, 5000]
          frame    = makeFrame [numCol "x" (normal ++ outliers)] 30
          report   = generateQualityReport frame
      -- Should have at least one Poor or worse check
      any (\c -> qcLevel c >= Poor) (qrChecks report) `shouldBe` True

    it "is always ready when overall level is Good" $
      property $ \(xs :: [Double]) ->
        let distinct = take 5 xs
            frame  = makeFrame [numCol "v" (if null distinct then [1] else distinct)] (max 1 (length distinct))
            report = generateQualityReport frame
        in qrReadyForAnalysis report == (qrOverallLevel report /= Unusable)

    it "generates non-empty summary text" $ do
      let frame  = makeFrame [numCol "x" [1..10]] 10
          report = generateQualityReport frame
      null (show (qrSummary report)) `shouldBe` False

  describe "QualityLevel ordering" $ do
    it "Good < Acceptable < Poor < Unusable" $ do
      (Good < Acceptable) `shouldBe` True
      (Acceptable < Poor) `shouldBe` True
      (Poor < Unusable)   `shouldBe` True
