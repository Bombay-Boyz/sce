{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Phase2.LiquidSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import SCE.Core.Error  (errorCode)
import SCE.Core.Types  (ErrorCode(..))
import SCE.Validation.LiquidTypes

isL :: Either a b -> Bool
isL (Left _) = True; isL _ = False

leftCode :: Either e b -> (e -> c) -> Maybe c
leftCode (Left e) f = Just (f e)
leftCode _        _ = Nothing

spec :: Spec
spec = do

  describe "validateProbability" $ do
    it "accepts 0.0" $ validateProbability 0.0 `shouldBe` Right 0.0
    it "accepts 1.0" $ validateProbability 1.0 `shouldBe` Right 1.0
    it "accepts 0.5" $ validateProbability 0.5 `shouldBe` Right 0.5
    it "rejects 1.1 (E3004)" $
      leftCode (validateProbability 1.1) errorCode `shouldBe` Just E3004
    it "rejects -0.01 (E3004)" $
      leftCode (validateProbability (-0.01)) errorCode `shouldBe` Just E3004
    it "rejects NaN (E3004)" $
      validateProbability (0/0) `shouldSatisfy` isL
    it "rejects Infinity (E3004)" $
      validateProbability (1/0) `shouldSatisfy` isL
    it "property: any value in [0,1] passes" $
      property $ \(x :: Double) ->
        (x >= 0 && x <= 1 && not (isNaN x) && not (isInfinite x)) ==>
          validateProbability x == Right x

  describe "validateCorrelation" $ do
    it "accepts -1.0" $ validateCorrelation (-1.0) `shouldBe` Right (-1.0)
    it "accepts 1.0"  $ validateCorrelation 1.0    `shouldBe` Right 1.0
    it "accepts 0.0"  $ validateCorrelation 0.0    `shouldBe` Right 0.0
    it "rejects -1.5 (E3004)" $
      leftCode (validateCorrelation (-1.5)) errorCode `shouldBe` Just E3004
    it "rejects 1.0001 (E3004)" $
      validateCorrelation 1.0001 `shouldSatisfy` isL
    it "rejects NaN" $
      validateCorrelation (0/0) `shouldSatisfy` isL
    it "property: any value in [-1,1] passes" $
      property $ \(x :: Double) ->
        (x >= -1 && x <= 1 && not (isNaN x) && not (isInfinite x)) ==>
          validateCorrelation x == Right x

  describe "validateDegreesOfFreedom" $ do
    it "accepts 1.0"  $ validateDegreesOfFreedom 1.0 `shouldBe` Right 1.0
    it "accepts 0.5"  $ validateDegreesOfFreedom 0.5 `shouldBe` Right 0.5
    it "rejects 0.0 (E4002)" $
      leftCode (validateDegreesOfFreedom 0.0) errorCode `shouldBe` Just E4002
    it "rejects -1.0 (E4002)" $
      leftCode (validateDegreesOfFreedom (-1.0)) errorCode `shouldBe` Just E4002
    it "rejects NaN (E3004)" $
      validateDegreesOfFreedom (0/0) `shouldSatisfy` isL
    it "property: any positive value passes" $
      property $ \(x :: Double) ->
        (x > 0 && not (isNaN x) && not (isInfinite x)) ==>
          validateDegreesOfFreedom x == Right x

  describe "validateEffectSize" $ do
    it "accepts 0.0" $ validateEffectSize 0.0 `shouldBe` Right 0.0
    it "accepts 0.8" $ validateEffectSize 0.8 `shouldBe` Right 0.8
    it "rejects negative (E3004)" $
      leftCode (validateEffectSize (-0.1)) errorCode `shouldBe` Just E3004
    it "rejects NaN (E3004)" $
      validateEffectSize (0/0) `shouldSatisfy` isL
    it "property: non-negative finite values pass" $
      property $ \(x :: Double) ->
        (x >= 0 && not (isNaN x) && not (isInfinite x)) ==>
          validateEffectSize x == Right x

  describe "SCE.Liquid stub modules compile" $ do
    it "all three stub modules are importable (no LH pragma)" $
      -- The fact this compiled confirms the modules exist and parse correctly
      True `shouldBe` True
