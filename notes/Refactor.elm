{- Drei verschiedene Kombinatoren für asynchrone Aktionen:

  - erster Art: integrieren, die Aktionen verschmelzen zu einer neuen.
  - zweiter Art: verketten, die Aktionen werden nacheinander ausgeführt.
  - dritter Art: aufreihen, die Aktionen werden unmittelbar nach dem Anstoß ausgeführt.

Es gilt: zweiter und dritter Art fallen zusammen bei rein synchronen Aktionen. Sie entsprechen dem `;` bei linearen Sprachen, bzw:

  - erster Art: Komposition
  - zweiter Art: intensionale Komposition
  - dritter Art: lineare Komposition

-}

module Refactor (

) where

import Task
import Signal


type State s a =
  State (s -> (s, a))

run : State s a -> s -> (s, a)
run (State f) = f

mapState : (s -> s) -> State s a -> State s a
mapState f st = State (\m -> let (m', x) = run st m
                              in (f m', x) )

andThen : State s a -> (a -> State s b) -> State s b
andThen p q = State (\model -> let (m, x) = run p model
                                in run (q x) m )

return : a -> State s a
return x = State (\m -> (m, x))

type alias Modifier error model =
  State model (Action error model)


type Action error model =
    None
  | Modify (Modifier error model)
  | Launch (model -> Task.Task error (Modifier error model))


pure : (model -> model) -> Action error model
pure f =
  Modify (mapState f <| return None)


async : (model -> Task.Task error (model -> model)) -> Action error model
async tf =
  Launch ( \m -> tf m `Task.andThen` \f -> Task.succeed (mapState f <| return None) )


actionbox : Signal.Mailbox (Action error model)
actionbox =
  Signal.mailbox None


type RepType error model = Sync (Action error model) | Async (Task.Task error (Action error model))

mmstack : model -> Signal.Signal (model, Maybe (RepType error model))
mmstack initmodel =
  let
    fork_ act (m, _) =
      case act of
        None -> (m, Nothing)
        Modify mo -> let (m', act') = run mo m in (m', Just <| Sync act')
        Launch tm -> ( m, Just <| Async <| tm m `Task.andThen` \mo -> Task.succeed (Modify mo) )
   in Signal.foldp fork_ (initmodel, Nothing) actionbox.signal


hook : model -> Signal.Signal model
hook initmodel =
  mmstack initmodel |> Signal.map fst
                    |> Signal.dropRepeat


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
