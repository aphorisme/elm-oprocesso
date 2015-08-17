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


{-| 'invoke' is the direct way to invoke an action. -}
invoke : Action error model -> Task.Task x ()
invoke act =
  Signal.send actionbox.address act


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
ioport : model -> Signal (Task.Task x ())
ioport initmodel =
    mmstack initmodel |> Signal.map snd
                      --^ make it a stream of the latest async tasks
                      |> Signal.filterMap identity (Sync (pure identity))
                      --^ if there is none (last action was 'None') then skip,
                      -- also, take the 'RepType' out of the maybe monad.
                      |> Signal.map
                            ( \rtyp -> case rtyp of
                                        Sync  act  -> Signal.send actionbox.address <| act
                                        Async tact -> tact `Task.andThen` \act -> Signal.send actionbox.address act )



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
thenDo : Action error model -> Action error model -> Action error model
thenDo act1 act2 =
  case act1 of
    None        -> act2
    Modify mo   -> Modify <| mo `andThen` ( \act -> return <| thenDo act act2 )
    Launch tm   -> Launch <| \m -> (tm m) `Task.andThen` ( \mo -> Task.succeed <| (mo `andThen` \act -> return <| thenDo act act2) )


next : Action error model -> Action error model -> Action error model
next act1 act2 =
  case act1 of
    None        -> act2
    Modify mo   -> Modify <| mo `andThen` ( \act -> return <| next act act2 )
    Launch tm   -> Launch <| \m -> invoke act2 `Task.andThen` \_ -> (tm m)



-- onfail
onfail : Action error model -> (error -> Action x model) -> Action x model
onfail act1 eact2 =
  case act1 of
    Launch tm -> Launch <| \m -> Task.toResult (tm m) `Task.andThen`
                              \r -> case r of
                                Ok mo   -> Task.succeed <| return (onfail (Modify mo) eact2)
                                Err err -> Task.succeed <| return (eact2 err)
    None      -> None
    Modify mo -> Modify <| mo `andThen` \act -> return (onfail act eact2)

onsuccess : Action error model -> Action x model -> Action x model
onsuccess act1 act2 =
  case act1 of
    None      -> None
    Modify mo -> Modify <| mo `andThen` \act -> return (onsuccess act act2)
    Launch tm -> Launch <| \m -> Task.toResult (tm m) `Task.andThen`
                             \r -> case r of
                               Ok mo   -> Task.succeed <| mo `andThen` (\act -> return <| onsuccess act act2)
                               Err err -> Task.succeed <| return None
