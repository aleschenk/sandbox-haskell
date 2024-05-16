{-# LANGUAGE ExtendedDefaultRules #-}

module LedgerAccountSpec where

import Test.Hspec
import Budget

spec :: Spec
spec = do
  describe "LedgerAccount" $ do
    it "creates an account correctly" $ do
      let account = createBudget "Test" 100
      accountName account `shouldBe` "Test"
      balance account `shouldBe` 100