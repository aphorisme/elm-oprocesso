module Oprocesso.EDSL where


import Oprocesso exposing (..)

infixl 6 `thenDo`
infixl 6 >>-
(>>-) = thenDo

infixl 5 `onfail`
infixl 5 !<<
(!<<) = onfail

infixl 3 -<<
(-<<) = thenDo



infixl 2 `next`
infixl 2 <=>
(<=>) = next
