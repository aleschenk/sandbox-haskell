{-# LANGUAGE OverloadedStrings #-}
module RestServer where

import Web.Scotty.Trans

main :: IO ()
main = scottyT 3000 id $ do
    get "/:word" $ do
        beam <- captureParam "word"
        text (beam <> " Scotty!")