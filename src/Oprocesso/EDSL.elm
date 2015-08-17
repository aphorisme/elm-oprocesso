module Oprocesso.EDSL where


import Oprocesso exposing (..)

infixl 5 `thenDo`
infixl 5 >>-
(>>-) = thenDo

infixl 4 `onfail`
infixl 4 !<<
(!<<) = onfail

infixl 3 -<<
(-<<) = thenDo



infixl 2 `next`
infixl 2 <=>
(<=>) = next
