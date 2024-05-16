module Budget where

data Budget = Budget
  { budgetName :: String
  , categories :: [Category]
  , total :: Int
  } deriving (Show, Eq)

data Category = Category
  { categoryName :: String
    , transactions :: [Transaction]
    , balance :: Int
    } deriving (Show, Eq)

data Transaction = Transaction
    { date :: String
        , description :: String
        , amount :: Int
        } deriving (Show, Eq)

-- | Create a new Budget with the given name and categories.
--
-- >>> createBudget "Cash" [] 0
-- Budget {budgetName = "Cash", categories = [], total = 0}
createBudget :: String -> [Category] -> Int -> Budget
createBudget = Budget

-- | Add the given category to the budget's categories.
--
-- >>> addCategory (Category "Cash" [] 0) (createBudget "Cash" [] 0)
-- Budget {budgetName = "Cash", categories = [Category {categoryName = "Cash", transactions = [], balance = 0}], total = 0}
addCategory :: Category -> Budget -> Budget
addCategory category budget = budget { categories = category : categories budget, total = total budget + balance category }
