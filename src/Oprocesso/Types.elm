module Oprocesso.Types where
{-|
  # Base Types

  ## State Monad
  @docs State, run, mapState, andThen, return

  ## Action and Modifier
  @docs Modifier, Action
-}


-- core:
import  Task


--////////////////--
--   BASE TYPES   --
--////////////////--

-----------------
-- State Monad

-- | the 'State' monad is the underlying type of a 'Modifier', where its bindable variable will be an 'Action.'
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
-- Action and Modifier

-- | a 'Modifier' is the core of an 'Action'. Inhabitants define how the model gets changed and which is the next 'Action' which has to happen (in the same synchronous task).
type alias Modifier error model =
  State model (Action error model)

-- | an 'Action' defines how the model change (if at all) or if an asynchronous modification shall take place.
type Action error model =
    None
  | Modify (Modifier error model)
  | Launch (model -> Task.Task error (Modifier error model))
