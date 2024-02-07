{-# LANGUAGE OverloadedStrings #-}
module RestServer where

import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
    get "/:word" $ do
        beam <- param "word"
        text (beam <> " Scotty!")