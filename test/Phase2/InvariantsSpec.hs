{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Phase2.InvariantsSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Vector as V
import           Data.Text   (Text)

import SCE.Core.Error  (errorCode)
import SCE.Core.Types  (MeasurementScale(..), ErrorCode(..), DataValue(..))
import SCE.Ingestion.Coercion
  (ValidatedFrame(..), TypedColumn(..), CoercionWarning(..))
import SCE.Ingestion.Schema   (CommittedSchema(..), CommittedColumn(..))
import SCE.Ingestion.MissingData (MissingnessReport(..))
import SCE.Validation.Invariants

-- Helpers
emptySchema :: CommittedSchema
emptySchema = CommittedSchema { csCommittedColumns = V.empty, csSource = Nothing }

emptyMissing :: MissingnessReport
emptyMissing = MissingnessReport [] 0 0 ""

makeFrame :: [TypedColumn] -> Int -> ValidatedFrame
makeFrame cols n = ValidatedFrame
  { vfColumns = V.fromList cols, vfRowCount = n
  , vfSource = Nothing, vfSchema = emptySchema
  , vfMissing = emptyMissing, vfWarnings = [] }

numCol :: Text -> [Double] -> TypedColumn
numCol name xs = NumericCol name Ratio (V.fromList xs)

isL :: Either a b -> Bool
isL (Left _) = True; isL _ = False

-- Extract error code from a Left value
leftCode :: Either e b -> (e -> c) -> Maybe c
leftCode (Left e) f = Just (f e)
leftCode _        _ = Nothing

spec :: Spec
spec = do

  describe "checkMonotonicity" $ do
    it "accepts strictly increasing sequence" $
      checkMonotonicity "ts" (V.fromList [1,2,3,4,5]) `shouldBe` Right ()
    it "rejects equal consecutive values (E2007)" $ do
      let r = checkMonotonicity "ts" (V.fromList [1,2,2,4])
      r `shouldSatisfy` isL
      leftCode r errorCode `shouldBe` Just E2007
    it "rejects decreasing values (E2007)" $
      leftCode (checkMonotonicity "ts" (V.fromList [5,3,1])) errorCode `shouldBe` Just E2007
    it "accepts empty vector" $
      checkMonotonicity "ts" V.empty `shouldBe` Right ()
    it "accepts single-element vector" $
      checkMonotonicity "ts" (V.fromList [42]) `shouldBe` Right ()

  describe "checkSumConstraint" $ do
    let a = numCol "a" [10, 20, 30]
        b = numCol "b" [5,  10, 15]
        t = numCol "t" [15, 30, 45]
        frame = makeFrame [a, b, t] 3

    it "passes when parts sum exactly to whole" $
      checkSumConstraint ["a","b"] "t" 0.01 frame `shouldBe` Right ()
    it "fails when sum is wrong (E2005)" $
      leftCode (checkSumConstraint ["a"] "t" 0.01 (makeFrame [numCol "a" [10], numCol "t" [99]] 1))
        errorCode `shouldBe` Just E2005
    it "fails with E2004 when a column is missing" $
      leftCode (checkSumConstraint ["x","b"] "t" 0.01 frame) errorCode `shouldBe` Just E2004
    it "passes within floating-point tolerance (0.01)" $ do
      let f = makeFrame [numCol "a" [0.1, 0.2], numCol "b" [0.2, 0.3], numCol "t" [0.3, 0.5]] 2
      checkSumConstraint ["a","b"] "t" 0.01 f `shouldBe` Right ()

  describe "checkUnitConsistency" $ do
    it "empty list gives no violations" $
      checkUnitConsistency [] `shouldBe` []
    it "no units detected — no violations" $
      checkUnitConsistency [("revenue", Nothing), ("cost", Nothing)] `shouldBe` []
    it "all same unit — no violations" $
      checkUnitConsistency [("revenue_usd", Nothing), ("cost_usd", Nothing)] `shouldBe` []
    it "different units — one violation" $ do
      let vs = checkUnitConsistency [("revenue_usd", Nothing), ("cost_eur", Nothing)]
      length vs `shouldBe` 1
      ivInvariant (head vs) `shouldBe` "unit-consistency"
    it "explicit overrides are respected" $ do
      let vs = checkUnitConsistency [("x", Just "USD"), ("y", Just "EUR")]
      length vs `shouldBe` 1

  describe "checkSignConstraints" $ do
    it "empty specs list — no violations" $
      checkSignConstraints V.empty (makeFrame [numCol "revenue" [-1, 2]] 2) `shouldBe` []
    it "warns when revenue column has negatives" $ do
      let frame  = makeFrame [numCol "revenue" [100, -50, 200]] 3
          specs  = V.fromList [("revenue", Ratio)]
          result = checkSignConstraints specs frame
      length result `shouldBe` 1
      ivInvariant (head result) `shouldBe` "sign-constraint"
    it "no violation when sign-sensitive column is all non-negative" $
      checkSignConstraints (V.fromList [("price", Ratio)])
        (makeFrame [numCol "price" [10, 20, 30]] 3) `shouldBe` []
    it "no violation for non-sign-sensitive column name" $
      checkSignConstraints (V.fromList [("widget_id", Nominal)])
        (makeFrame [numCol "widget_id" [-1, 2]] 2) `shouldBe` []

  describe "existing checks still work" $ do
    it "checkPercentageInvariants passes for valid percentages" $
      checkPercentageInvariants (V.fromList [25,25,25,25]) `shouldBe` Right ()
    it "checkPercentageInvariants fails for bad sum (E2005)" $
      leftCode (checkPercentageInvariants (V.fromList [30,30,30])) errorCode `shouldBe` Just E2005
    it "checkTimeSeriesInvariants fails on empty (E2002)" $
      leftCode (checkTimeSeriesInvariants V.empty) errorCode `shouldBe` Just E2002
    it "checkRangeInvariants fails on out-of-range value (E2001)" $
      leftCode (checkRangeInvariants 0 10 (V.fromList [5, 15])) errorCode `shouldBe` Just E2001

  describe "properties" $ do
    it "checkMonotonicity: ascending list always passes" $
      property $ \(xs :: [Double]) ->
        -- Build a strictly ascending list by using cumulative sum of (|x|+1),
        -- guaranteeing each step is at least +1 so no two values are equal.
        let steps = map ((+ 1) . abs) xs
            strictly_ascending = scanl (+) 0 steps
            vec = V.fromList strictly_ascending
        in checkMonotonicity "ts" vec == Right ()
