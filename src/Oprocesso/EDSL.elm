module Oprocesso.EDSL where
{-| The `EDSL` module provides infix operator symbols and precedence to use the framework as mentioned in the `README`.
Just include, if you into such stuff.

@docs (>>-), (!<<), (-<<), (=>>)
-}

import Oprocesso exposing (thenDo, onfail, next)
import Oprocesso.Types exposing (Action)

infixl 5 `thenDo`
infixl 5 >>-
{-| Infix operator for 'thenDo'. -}
(>>-) : Action error model -> Action error model -> Action error model
(>>-) = thenDo

infixl 4 `onfail`
infixl 4 !<<
{-| Infix operator for 'onfail'. -}
(!<<) : Action error model -> (error -> Action x model) -> Action x model
(!<<) = onfail

infixl 3 -<<
{-| Infix operator for 'thenDo' with *lower* fixity then '(>>-)' or 'thenDo'. -}
(-<<) : Action error model -> Action error model -> Action error model
(-<<) = thenDo



infixl 2 `next`
infixl 2 =>>
{-| Infix operator for 'next'. -}
(=>>) : Action error model -> Action error model -> Action error model
(=>>) = next
