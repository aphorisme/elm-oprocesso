module Oprocesso where
{-|
  # Framework
  @docs hook, actionbox

  ## Ports
  @docs ioport, unbatch



  # Combinators

  ## Simple building blocks
  @docs pure, purelift, async, with

  ## Flow Control
  @docs thenDo, incorpl, incorpr, onsuccess
-}


-- core:
import  Task                    exposing (Task)
import  Signal                  exposing (Signal, Mailbox, mailbox)

-- intern:
import  Oprocesso.Types         exposing (..)
import  Oprocesso.Background    exposing (mmstack, operationbox)



--///////////--
-- FRAMEWORK --
--///////////--

{-| 'hook' sets up the main model signal.

  main = Signal.map view (hook initmodel)
-}
hook : model -> Signal model
hook initmodel =
  Signal.map fst (mmstack initmodel)

{-| 'ioport' sets up a port which runs the asynchronous tasks and keeps feeding them back into the 'actionbox'.

  import Oprocesso.Framework as OPF

  port asyncrunner : Signal (Task x ())
  port asyncrunner = OPF.ioport initmodel errorHandler


-}
ioport : model -> (error -> (Modifier model)) -> Signal (Task x ())
ioport initmodel errorHandler =
    mmstack initmodel |> Signal.map snd
                      --^ make it a stream of the latest async tasks
                      |> Signal.filterMap identity (Task.succeed identity)
                      --^ if there is none (last action was pure) then skip,
                      -- also, take the task out of the maybe monad.
                      |> Signal.map Task.toResult
                      |> Signal.map (\t -> t `Task.andThen` (\rt -> case rt of
                            Ok f    -> Signal.send actionbox.address <| pure f
                            Err err -> Signal.send actionbox.address <| pure (errorHandler err) ) )


{-| 'actionbox' is where one sends actions to, when using the provided framework.
-}
actionbox : Mailbox (Action model error)
actionbox =
  mailbox []

unbatch : Signal (Task x ())
unbatch =
  let
    unbatch_ actions =
      case actions of
        []       -> Signal.send operationbox.address (\m -> Pure m)
        [o]      -> Signal.send operationbox.address o
        (o::os)  -> Signal.send operationbox.address o `Task.andThen` (\_ -> unbatch_ os)
  in
    Signal.map unbatch_ actionbox.signal



--/////////////--
-- COMBINATORS --
--/////////////--

-------------------------
-- simple building blocks


{-| The easiest building blocks are actions which just change the model, these are so called 'pure' ones.

  pure (\m -> { m | entries <- asEntry m.typed
                  , typed   <- ""})
-}
pure : Modifier model -> Action model error
pure f =
  [\m -> Pure (f m)]


purelift : (a -> Modifier model) -> (a -> Action model error)
purelift f =
  \x -> (pure <| f x)

{-| An action might be based on a simple task which does not depend on the current model; this is when 'async' can be used.

  async refreshWeatherStatus

-}
async : Task error (Modifier model) -> Action model error
async t =
  [\_ -> Async t]

{-| Often one wants to base an action on a task, where this task needs some information from the current model, for example a http request which needs a session id. 'with' is the way to go:

  httpRequest `with` .sessionId

-}
with : (a -> AsyncModifier model error) -> (model -> a) -> Action model error
with ft acc =
  [\m -> Async (ft <| acc m)]




-------------------
-- flow control

{-| Action can be combined with 'thenDo' which sets them up in a temporal order. One can use '(==>)' for an infix operator.

        pure (print "start request")
    ==> pure (putOnHold)
    ==> httpRequest `with` .sessionId

Though, it does not wait until an asynchronous operation returns. See 'incorpr' and 'onsuccess' for such functions.
-}
thenDo : Action model error -> Action model error -> Action model error
thenDo = (++)

infix 5 ==>
(==>) = thenDo

{-| If one wants to modify the model *before* a certain action happens, one can use 'incorpl' to glue a modification and an action together:

  print "Starting Request." `incorpl` (httpRequest `with` .sessionId)

For a more eDSL like feeling, one can use '(>>-)' also:

      print "Starting Request."
  >>- putOnHold
  >>- (httpRequest `with` .sessionId)

This is in fact nothing else but lifting the modifier and `thenDo` the given action.
-}
incorpl : Modifier model -> Action model error -> Action model error
incorpl puref act =
  pure puref `thenDo` act


infixr 6 >>-
(>>-) = incorpl

{-| To modify an action on the right (i.e. changing the result after it got executed) one can use 'incorpr'. It changes to last operation of the action in such a way, that the given modification of the model takes place *right after* the action has successfully happened or the error handler has modified the model. For asynchronous actions this means that the modification will happen right after the asynchronous action has been returned and applied (if it succeded) or *before* the error handler modified the model.

  (httpRequest `with` .sessionId) `incorpl` setupForNextInput

For a more eDSL like feeling (and for mixing with other operators) there is the infix operator '(-<<)':

      print "start request"
  >>- putOnHold
  >>-
      httpRequest `with` .sessionId
  -<<
      removeHold

The difference between 'thenDo' and 'incorpr' is the moment when the second actions happens. For 'thenDo' it does after starting the asynchronous operation, where the modification given to 'incorpr' starts when (any) result from the asynchronous operation did come back.
-}
incorpr : Action model error -> Modifier model -> Action model error
incorpr act puref =
  let l = List.length act
  in case List.drop (l - 1) act of
  []   -> []
  [op] ->    List.take (l - 1) act
             ++ [\m -> case op m of
                    Pure m' -> Pure (puref m')
                    Async t -> Async <| Task.toResult t `Task.andThen`
                                \r -> case r of
                                    Ok f     -> Task.succeed (f >> puref)
                                    -- the following is cheating. I know.
                                    Err err  ->      Signal.send operationbox.address (\m -> Pure (puref m))
                                      `Task.andThen` (\_ -> Task.fail err) ]


infixl 7 -<<
(-<<) = incorpr

{-| 'onsuccess' follows the idea of 'incorpr', but the modifier will only happen, if the asynchronous one did succeed. One can use '(-<!)' as an infix operator for this operation. It should appear before any '(-<<)':

        print "start request"
    >>- putOnHold
    >>-
        httpRequest `with` .sessionId
        -<! print "we have a success!"
    -<<
        removeHold -}
onsuccess : Action model error -> Modifier model -> Action model error
onsuccess act puref =
  let l = List.length act
  in case List.drop (l - 1) act of
  []   -> []
  [op] ->    List.take (l - 1) act
             ++ [\m -> case op m of
                    Pure m' -> Pure (puref m')
                    Async t -> Async (Task.map (\f -> f >> puref) t)]


infixl 8 -<!
(-<!) = onsuccess
