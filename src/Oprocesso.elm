module Oprocesso where
{-|
  # Framework
  @docs actionbox, hook, ioport

  # Lifts
  @docs pure, async, asyncOn, task

  # Combinators
-}

--- Intern:
import    Oprocesso.Types         exposing (..)
--- Core:
import    Task
import    Signal


--//////////////--
--  BACKGROUND  --
--//////////////--
type RepType error model =
    Sync  (Action error model)
  | Async (Task.Task error (Action error model))

{-| The 'mmstack' is a signal consisting of the latest model and the next action, if there is any. It *forks* the actionbox by past folding pure operations and keeping track of the latest next action.
-}
mmstack : model -> Signal.Signal (model, Maybe (RepType error model))
mmstack initmodel =
  let
    fork_ act (m, _) =
      case act of
        None -> (m, Nothing)
        Modify mo -> let (m', act') = run mo m in (m', Just <| Sync act')
        Launch tm -> ( m, Just <| Async <| tm m `Task.andThen` \mo -> Task.succeed (Modify mo) )
   in Signal.foldp fork_ (initmodel, Nothing) actionbox.signal


--///////////--
-- FRAMEWORK --
--///////////--

{-| 'actionbox' is where one sends actions to, when using the provided framework. -}
actionbox : Signal.Mailbox (Action error model)
actionbox =
  Signal.mailbox None



{-| 'hook' sets up the main model signal.

  import Oprocesso  as O
  main = Signal.map view (O.hook initmodel)

-}
hook : model -> Signal.Signal model
hook initmodel =
  mmstack initmodel |> Signal.map fst
                    |> Signal.dropRepeats


{-| 'ioport' sets up a port which runs the asynchronous tasks and keeps feeding them back into the 'actionbox'.

  import Oprocesso as O

  port asyncrunner : Signal (Task x ())
  port asyncrunner = O.ioport initmodel errorHandler


-}
ioport : model -> (error -> (model -> model)) -> Signal (Task.Task x ())
ioport initmodel errorHandler =
    mmstack initmodel |> Signal.map snd
                      --^ make it a stream of the latest async tasks
                      |> Signal.filterMap identity (Sync (pure identity))
                      --^ if there is none (last action was 'None') then skip,
                      -- also, take the 'RepType' out of the maybe monad.
                      |> Signal.map
                            ( \rtyp -> case rtyp of
                                        Sync  act  -> Signal.send actionbox.address <| act
                                        Async tact -> Task.toResult tact
                                                     `Task.andThen` \ract -> case ract of
                                                                        Ok act -> Signal.send actionbox.address act
                                                                        Err err -> Signal.send actionbox.address (pure <| errorHandler err) )


--/////////////--
--    LIFTS    --
--/////////////--

{-| The easiest building blocks are actions which just change the model, these are so called 'pure' ones.

  pure (\m -> { m | entries <- asEntry m.typed
                  , typed   <- ""})
-}
pure : (model -> model) -> Action error model
pure f =
  Modify (mapState f <| return None)


{-| An asynchronous modification of the model is any task which gets invoked based on the current model and returns with a modification. One can use 'async' to lift such functions. -}
async : (model -> Task.Task error (model -> model)) -> Action error model
async tf =
  Launch ( \m -> tf m `Task.andThen` \f -> Task.succeed (mapState f <| return None) )

{-| Often the invocation of tasks depends on a certain value out of the model which can be reached by a getter. 'asyncOn' is meant to be used in such a situation:

  requestJson `asyncOn` .typed


-}
asyncOn : (a -> Task.Task error (model -> model)) -> (model -> a) -> Action error model
asyncOn tf getter =
  async (\m -> tf (getter m))


{-| An asynchronous modification which does not depend on the current model can be lifted with 'task'.
-}
task : (Task.Task error (model -> model)) -> Action error model
task t =
  async <| \_ -> t

--/////////////--
-- COMBINATORS --
--/////////////--

-------------------------
-- simple building blocks
