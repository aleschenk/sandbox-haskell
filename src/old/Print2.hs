{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface, CPP #-}

module Print2 where

main :: IO()
main = do
  putStr "Count to four me:"
  putStrLn "one, two"
  putStrLn ", three, and"
  putStr " four!"


