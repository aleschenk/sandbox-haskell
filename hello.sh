#!/usr/bin/env cabal
{- cabal:
build-depends: base, split
-}

import Data.List.Split (chunksOf)

main :: IO ()
main = getLine >>= print . chunksOf 3