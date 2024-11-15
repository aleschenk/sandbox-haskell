{-# LANGUAGE ExtendedDefaultRules #-}

module BudgetSpec where

import Test.Hspec ( Spec, describe, it, shouldBe )
import Budget ( createBudget, Budget(budgetName) )

spec :: Spec
spec = do
  describe "LedgerAccount" $ do
    it "creates an account correctly" $ do
      let budget = createBudget "Test" [] 100
      budgetName budget `shouldBe` "Test"