{-# LANGUAGE ExtendedDefaultRules #-}

module LedgerAccountSpec where

import Test.Hspec
import LedgerAccount

spec :: Spec
spec = do
  describe "LedgerAccount" $ do
    it "creates an account correctly" $ do
      let account = createAccount "Test" 100
      accountName account `shouldBe` "Test"
      balance account `shouldBe` 100

    it "deposits correctly" $ do
      let account = createAccount "Test" 100
      let updatedAccount = deposit 50 account
      balance updatedAccount `shouldBe` 150

    it "withdraws correctly" $ do
      let account = createAccount "Test" 100
      let updatedAccount = withdraw 50 account
      balance updatedAccount `shouldBe` 50

    it "transfers correctly" $ do
      let account1 = createAccount "Test1" 100
      let account2 = createAccount "Test2" 0
      let (updatedAccount1, updatedAccount2) = transfer 50 account1 account2
      balance updatedAccount1 `shouldBe` 50
      balance updatedAccount2 `shouldBe` 50

    it "gets an account correctly" $ do
      let account1 = createAccount "Test1" 100
      let account2 = createAccount "Test2" 0
      let accounts = [account1, account2]
      let retrievedAccount = getAccount "Test1" accounts
      retrievedAccount `shouldBe` Just account1

    it "gets an account that not exists" $ do
      let account1 = createAccount "Test1" 100
      let accounts = [account1]
      let retrievedAccount = getAccount "Test2" accounts
      retrievedAccount `shouldBe` Nothing
