module Oprocesso.EDSL where
{-| The `EDSL` module provides infix operator symbols and precedence to use the framework as mentioned in the `README`.
    Just include, if you into such stuff.

@docs >>-, !<<, -<<, =>>
-}

import Oprocesso exposing (thenDo, onfail, next)

infixl 5 `thenDo`
infixl 5 >>-
(>>-) = thenDo

infixl 4 `onfail`
infixl 4 !<<
(!<<) = onfail

infixl 3 -<<
(-<<) = thenDo



infixl 2 `next`
infixl 2 =>>
(=>>) = next
