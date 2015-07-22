module Oprocesso.Types where
{-|
  # Base Types
  @docs Action, Operation, OperationMode, Modifier, AsyncModifier

-}


-- core:
import  Task        exposing (Task)


--////////////////--
--   BASE TYPES   --
--////////////////--

type OperationMode model error =
    Pure  model
  | Async (AsyncModifier model error)

type alias Operation model error =
  model -> OperationMode model error


type alias Action model error =
  List (Operation model error)


type alias Modifier model =
  model -> model

type alias AsyncModifier model error =
  Task error (Modifier model)
