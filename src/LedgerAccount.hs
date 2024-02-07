module LedgerAccount where

type AccountName = String

data LedgerAccount = LedgerAccount
  { accountName :: AccountName
  , balance :: Int
  } deriving (Show, Eq)


-- | Create a new account with the given name and balance.
--
-- >>> createAccount "Cash" 100
-- LedgerAccount {accountName = "Cash", balance = 100}
createAccount :: String -> Int -> LedgerAccount
createAccount name balance = LedgerAccount name balance

-- | Add the given amount to the account's balance.
--
-- >>> deposit 100 (createAccount "Cash" 100)
-- LedgerAccount {accountName = "Cash", balance = 200}
deposit :: Int -> LedgerAccount -> LedgerAccount
deposit amount account = account { balance = balance account + amount }

-- | Subtract the given amount from the account's balance.
--
-- >>> withdraw 100 (createAccount "Cash" 100)
-- LedgerAccount {accountName = "Cash", balance = 0}
withdraw :: Int -> LedgerAccount -> LedgerAccount
withdraw amount account = account { balance = balance account - amount }

-- | Transfer the given amount from the first account to the second account.
--
-- >>> transfer 100 (createAccount "Cash" 100) (createAccount "Bank" 0)
-- (LedgerAccount {accountName = "Cash", balance = 0},LedgerAccount {accountName = "Bank", balance = 100})
transfer :: Int -> LedgerAccount -> LedgerAccount -> (LedgerAccount, LedgerAccount)
transfer amount from to = (withdraw amount from, deposit amount to)

-- | Return the account with the given name.
--
-- >>> getAccount "Cash" [createAccount "Cash" 100, createAccount "Bank" 0]
-- NOW Just (LedgerAccount {accountName = "Cash", balance = 100})
getAccount :: AccountName -> [LedgerAccount] -> Maybe LedgerAccount
-- getAccount name accounts = head <$> filter (\account -> accountName account == name) accounts
getAccount name accounts = Just (head (filterAccount name accounts))


filterAccount :: AccountName -> [LedgerAccount] -> [LedgerAccount]
filterAccount name = filter ((== name) . accountName)
