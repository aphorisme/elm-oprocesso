module Oprocesso.Background where
{-|
    # Background
    @docs operationbox, mmstack
-}

-- core:
import  Signal            exposing (Mailbox, mailbox, Signal, foldp)
import  Task              exposing (Task)

-- intern:
import  Oprocesso.Types   exposing (..)



--//////////////--
--  BACKGROUND  --
--//////////////--


{-| Every 'Action' is actually a list of 'Operation's. This list gets "unbatched" first, i.e. every operation gets send to the 'operationbox', from where the 'mmstack' takes them. -}
operationbox : Mailbox (Operation model error)
operationbox =
  mailbox (\m -> Pure m)


{-| The 'mmstack' is a signal consisting of the latest model and the latest asynchronous task, if there is any. It *forks* the actionbox by past folding pure operations and keeping track of the latest asynchronous task.
-}
mmstack : model -> Signal (model, Maybe (AsyncModifier model error))
mmstack initmodel =
  let
    fork_ op (m, _) =
      case op m of
        Pure m' -> (m', Nothing)
        Async t -> (m , Just t)
  in
    foldp fork_ (initmodel, Nothing) operationbox.signal
