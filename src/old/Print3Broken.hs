module Print3Broken where

printSecond :: IO()
printSecond = do
  putStrLn greeting

greeting = "Yarrrr"

addExclamationMark text = text ++ "!"

thirdLetter :: String -> Char
thirdLetter text = text !! 2

returnY :: String -> String
returnY text =  woot text
                where woot :: String -> String
                      woot text = take 1 $ drop 4 text


rvrs = putStrLn $ awesome ++ " " ++ is ++ " " ++ curry
        where  text = "Curry is awesome"
               curry = take 5 text
               is = take 2 $ drop 6 text
               awesome = drop 9 text

--rvrs = let mesage = "Curry is awesome"
--            curry = take 5 message
--       in text = curry ++ "a"

-- Curry is awesome

main :: IO()
main = do
  putStrLn greeting
  printSecond
