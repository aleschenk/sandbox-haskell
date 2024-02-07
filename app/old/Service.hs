module Service where

import Lib

-- El valor 1 es un literal y al mismo tiempo una expresion irreducible
-- Decimos que uno esta en su forma normal
one = 1

triple x = x * 3

quad x = x * 4

calc x = 3.14 * (x * x)

sum2 x = x + x

main :: IO ()
main = someFunc
