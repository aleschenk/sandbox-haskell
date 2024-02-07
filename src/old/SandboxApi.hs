{-# LANGUAGE OverloadedStrings #-}

module SandboxApi where

import Web.Scotty

main = do
  putStrLn "Starting Server..."
  scotty 3000 $ do
    get "/hello" $ do
      text "hello world!"
