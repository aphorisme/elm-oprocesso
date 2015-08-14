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
