module Oprocesso.Types where
{-|
  ## State Monad
  @docs State, run, mapState, andThen, return

  ## Core Types
  @docs Modifier, Action
-}


-- core:
import  Task


--////////////////--
--   BASE TYPES   --
--////////////////--

-----------------
-- State Monad
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


------------------
-- Core Types
type alias Modifier error model =
  State model (Action error model)


type Action error model =
    None
  | Modify (Modifier error model)
  | Launch (model -> Task.Task error (Modifier error model))
