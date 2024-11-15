module Journal (Journal(..), Entry(..), createJournal, addEntry, getJournal, getOrCreateJournal) where

data Journal = Journal
  { journalName :: String
  , entries :: [Entry]
  , balance :: Int
  } deriving (Show, Eq)

data Entry = Entry
  { date :: String
    , description :: String
    , amount :: Int
    } deriving (Show, Eq)

-- | Create a new Journal with the given name and entries.
--
-- >>> createJournal "Cash" [] 0
-- Journal {journalName = "Cash", entries = [], balance = 0}
createJournal :: String -> [Entry] -> Int -> Journal
createJournal name entries balance = Journal name entries balance

-- | Add the given entry to the journal's entries.
--
-- >>> addEntry (Entry "2020-01-01" "Initial balance" 100) (createJournal "Cash" [] 0)
-- Journal {journalName = "Cash", entries = [Entry {date = "2020-01-01", description = "Initial balance", amount = 100}], balance = 100}
addEntry :: Entry -> Journal -> Journal
addEntry entry journal = journal { entries = entry : entries journal, balance = balance journal + amount entry }

-- | Return the journal with the given name.
--
-- >>> getJournal "Cash" [createJournal "Cash" [] 0, createJournal "Bank" [] 0]
-- Just (Journal {journalName = "Cash", entries = [], balance = 0})
getJournal :: String -> [Journal] -> Maybe Journal
getJournal name journals = Just (head (filterJournal name journals))

-- | Return the journal with the given name, or create a new one if it doesn't exist.
filterJournal :: String -> [Journal] -> [Journal]
filterJournal name = filter ((== name) . journalName)

-- | Return the journal with the given name, or create a new one if it doesn't exist.
--
-- >>> getOrCreateJournal "Cash" [createJournal "Bank" [] 0]
-- Journal {journalName = "Cash", entries = [], balance = 0}
getOrCreateJournal :: String -> [Journal] -> Journal
getOrCreateJournal name journals = case getJournal name journals of
  Just journal -> journal
  Nothing -> createJournal name [] 0

-- | Sum two integers.
-- >>> sumaa 1 4
-- 5
sumaa a b = a + b
