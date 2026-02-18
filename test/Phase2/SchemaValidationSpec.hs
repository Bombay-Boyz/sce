{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Phase2.SchemaValidationSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Vector as V
import           Data.Text   (Text)
import qualified Data.Text   as T

import SCE.Core.Error   (DetailedError, errorCode)
import SCE.Core.Types   (DataValue(..), MeasurementScale(..), ErrorCode(..))
import SCE.Validation.Schema

numVec :: [Double] -> V.Vector DataValue
numVec = V.fromList . map NumericValue

txtVec :: [Text] -> V.Vector DataValue
txtVec = V.fromList . map TextValue

isL :: Either a b -> Bool
isL (Left _) = True; isL _ = False

lCode :: Either DetailedError b -> Maybe ErrorCode
lCode (Left e) = Just (errorCode e); lCode _ = Nothing

spec :: Spec
spec = do

  describe "validateScale Ratio" $ do
    it "accepts non-negatives" $
      validateScale "price" Ratio (numVec [0,1,100]) `shouldBe` Right ()
    it "rejects negative value (E2003)" $
      lCode (validateScale "price" Ratio (numVec [1,-0.5,3])) `shouldBe` Just E2003
    it "accepts empty vector" $
      validateScale "x" Ratio V.empty `shouldBe` Right ()
    it "accepts MissingValue entries" $
      validateScale "x" Ratio (V.fromList [NumericValue 5, MissingValue]) `shouldBe` Right ()

  describe "validateScale Interval" $ do
    it "accepts negatives" $
      validateScale "temp" Interval (numVec [-40,0,37]) `shouldBe` Right ()

  describe "validateScale Nominal" $ do
    it "accepts low cardinality" $
      validateScale "status" Nominal (txtVec ["A","B","C"]) `shouldBe` Right ()
    it "rejects >50 unique values (E2008)" $
      lCode (validateScale "id" Nominal (txtVec (map (T.pack . show) [1..51::Int])))
        `shouldBe` Just E2008

  describe "validateCardinality" $ do
    it "passes at threshold" $
      validateCardinality "c" 50 (txtVec (map (T.pack . show) [1..50::Int])) `shouldBe` Right ()
    it "fails at threshold+1 (E2008)" $
      lCode (validateCardinality "c" 50 (txtVec (map (T.pack . show) [1..51::Int])))
        `shouldBe` Just E2008
    it "ignores MissingValue in unique count" $
      validateCardinality "c" 50
        (V.fromList (replicate 20 MissingValue ++ map (TextValue . T.pack . show) [1..5::Int]))
        `shouldBe` Right ()

  describe "validateColumn (total)" $ do
    it "no errors for clean Ratio column" $ do
      let r = validateColumn "price" Ratio (numVec [10,20,30])
      cvrErrors r `shouldBe` []
      cvrColumn r `shouldBe` "price"
    it "records error for negative Ratio" $
      cvrErrors (validateColumn "p" Ratio (numVec [1,-1])) `shouldSatisfy` (not . null)
    it "records warning for high-cardinality Nominal" $
      cvrWarnings (validateColumn "id" Nominal (txtVec (map (T.pack . show) [1..60::Int])))
        `shouldSatisfy` (not . null)
    it "is total (property)" $
      property $ \(n :: Int) ->
        let vec = V.fromList (map (NumericValue . fromIntegral) [1..max 0 n])
        in cvrColumn (validateColumn "col" Ratio vec) == "col"

  describe "properties" $ do
    it "Ratio rejects any vector with a negative" $
      property $ \(xs :: [Double]) ->
        not (null xs) && any (<0) xs ==>
          isL (validateScale "c" Ratio (V.fromList (map NumericValue xs)))
    it "Interval always accepts" $
      property $ \(xs :: [Double]) ->
        validateScale "c" Interval (V.fromList (map NumericValue xs)) == Right ()
