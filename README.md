# *oprocesso* - a combinator-based elm framework

> Josef K. foi certamente vítima de alguma calúnia, pois, numa bela manhã, sem ter feito nada de mal, foi detido.

## Disclaimer

This framework is at the moment at a state of a bare *proof of concept*. The chances are high that it will suffer significant changes. It might even be the case that the key idea is flawed, I don't know yet.

The library itself only depends on `elm-lang/core`. The example(s) added more constraints. Sorry, did not mess with these for now.

## Dive Into

This `README` is meant to be an overall overview; it gives the general concept, then the semantics and eventually the implementation details. I'll keep it updated as I go along.
Also, I tried to give examples within the documentation of the library itself. You might have a look (especially into `src\Oprocesso.elm`).

Besides I'll keep adding examples to `examples/` -- if you have some concept of the general model-view-controller style in `elm`, you should get the idea just by starring long enough into `examples/JsonEcho.elm`.

## Basic Concept

The framework *oprocesso* is meant to give a way to structure an elm app; especially it manages all the signal processing; all what is left is writing the meat of the app.

The concept is based on the strict model-view-controller framework which `elm` suggests, at least when building HTML apps (see [the TodoMVC example](https://github.com/evancz/elm-todomvc>)), but it shifts it in an important way: instead of defining an `Action` type, which inhabitant's get send to the action mailbox whenever there is some action happening, one defines `Oprocesso.Action`s which can be thought of as functions of type `Model -> Model` and which get instead send to the action mailbox.

So, instead of

```{.elm}
data Action = NoAction | Typing String | ...

update : Action -> Model -> Model
update act m =
  case act of
    NoAction   -> m
    Typing s   -> { m | typed <- s }
    ...
{-
  ...
-}  

inputfield : String -> Html
inputfield inp =
  input [ value inp
        , on "input" (Json.Decode.map Typing targetValue)
                     (Signal.message actionbox.address) ] []

{- ... -}
```

with *oprocesso* one can write:

```{.elm}
typing : String -> Modifier Model
typing s =
  \m -> { m | typed <- s }

{-
  ...
-}  

inputfield : String -> Html
inputfield inp =
  input [ value inp
        , on "input" (Json.Decode.map (purelift typing) targetValue)
                     (Signal.message actionbox.address) ] []
```

One might say it is just a *shift* from a first order `Action` to a second order `Model -> Model` which gets foldp'ed instead. This shift (which was actually brought up by Max Goldstein in his [answer](https://groups.google.com/forum/#!topic/elm-discuss/RFLjOxQp1PA) to a question of mine), although, leads to two points which I consider as an improvement (from now on, "Actions" are meant to be `Oprocesso.Action`s):

  1. Actions are functions; those functions can be combined in a natural way to form new functions, so starting with small, obvious actions, one can build up more complex ones -- which stay therefore obvious. (For those who had the time and pleasure to build their own [monadic] parser combinators [in Haskell], the shift described above might already have rang a bell. Although, `Oprocesso.Action`s are not in the same kind monadic as parser combinators are -- [they could, but they don't need to](#actionNOTmonadic).)
  2. Actions can be asynchronous; this is due to the fact, that actions can be made out of `Task`s.

## Framework's Entities

### Actions and Modifiers

In the center there is an `Oprocesso.Action model error`; it represents an action on the model, i.e. something that describes how to map a certain model onto another. There are two differnt sorts of actions:

  - **pure actions**: these just change the model, so they can be thought of as functions of type `Model -> Model`
  - **asynchronous actions**: these build up a *pure action* on the current model which gets executed asynchronously. They can be thought of functions of type `Model -> (Model -> Model)`.

Since *pure actions* are almost just functions of type `Model -> Model` they have a close relationship to such functions, which are called **modifiers** in *oprocesso*. Usually *pure actions* are made out of *modifiers* by lifting them with `pure` and `purelift`:

```{.elm}
addEntry : Oprocesso.Modifier Model
addEntry =
  \m -> { m | entries <- m.entries ++ [m.typed] }

setInput : String -> Oprocesso.Modifier Model
setInput s =
  \m -> { m | typed <- s }

{-
  Now we have:

    pure addEntry : Oprocesso.Action Model x

  and

    purelift setInput : String -> Oprocesso.Action Model x
-}
```

These actions are called *pure* since they do not involve any IO, any outside computation (that may fail). So, they are pure in a functional sense.

On the other hand, *asynchronous actions*  involving outside computation. They may send a http request, doing some database communicating etc. Hence they are related to `Task`s of a certain kind, such, which are modifying the model, i.e. of type `Task error (model -> model)`. These tasks are called **asynchronous modifiers** in *oprocesso* and are used to make up *asynchronous actions* with `async` and `with`:

```{.elm}
echoJson : Oprocesso.AsyncModifier Model String
echoJson =
  let vvDecoder_ =
        object2 (,)
          ("v1" := string)
          ("v2" := string)
  in Task.mapError toString (Http.get vvDecoder_ ("http://echo.jsontest.com/v1/Hello/v2/World"))
     >>= \(s1, s2) -> Task.succeed <| addEntry ("Echoed: " ++ s1 ++ "/" ++ s2)

asyncRequestJson : String -> Oprocesso.AsyncModifier Model String
asyncRequestJson typd =
  let vvDecoder_ =
        object2 (,)
          ("v1" := string)
          ("v2" := string)
  in Task.mapError toString (Http.get vvDecoder_ ("http://echo.jsontest.com/" ++ typd))
     >>= \(s1, s2) -> Task.succeed <| addEntry ("Echoed: " ++ s1 ++ "/" ++ s2)

{-
  Hence,

    async echoJson : Oprocesso.Action Model String

  and

    asyncRequestJson `with` .typed : Oprocesso.Action Model String

  where

    .typed : Model -> String
-}
```

So the function `pure` has its dual in `async`, where `with` is connected to `purelift` in some sense.

## Flow Control

Okay, so I've explained how to make up actions from simple building blocks; these so gained actions are somewhat overt. Also, *oprocesso* gives one the possibility to make up more complex *actions* by combining *modifiers*, *asynchronous modifiers* and *actions* themselves.

There are four flow control operators:

  1. `thenDo : Action model error -> Action model error -> Action model error` (or `(==>)`) which just glues two actions together such that they get executed one right after the other. (Which actually means that the second action does not wait until the asynchronous one has finished, if the first is of such kind.)
  2. `incorpl : Modifier model -> Action model error -> Action model error` (or `(>>-)`) which fiddels in a modifier right *before* an action,
  3. `incorpr : Action model error -> Modifier model -> Action model error` (or `(-<<)`) which fiddels in a modifier right *after* an action, i.e. it incorporates the modifier into the asynchronous action, or: it applies the modifier right after the asynchronous action has been applied, so to say, and
  4. `onsuccess : Action model error -> Modifier model -> Action model error` (or `(-<!)`) which fiddels in a modifier in such a way that it happens only, if the action before was successful.

The idea is to define the needed *modifiers* and *asynchronous modifiers* and then glue them together into complex *actions*:

```{.elm}

  {-| 'makeRequest'
  - adds the current input into the history,
  - starts an asynchronous request based on the current input
  >> (which pushes its result into the history when completed) and
  - empties the input box right after. -}

  makeRequest : Action Model String
  makeRequest =
             addTyped
                >>- asyncRequestJson `with` .typed
    `thenDo` pure (setInput "")

  {- ... used in the following way:

      onEnter (.address Oprocesso.actionbox) makeRequest

  -}
```

There are more examples within the documentation of the flow controllers in the `Oprocesso` module.

## Setup *oprocesso*

First, you need some imports:

```{.elm}
import  Oprocesso               exposing ({- which flow controllers you need -})
import  Oprocesso.Types         exposing (..)
```

where I propose: expose everything from `Oprocesso.Types` but just the flow controllers from `Oprocesso`.

Next, there are two ports you need to run:

```{.elm}
port asyncrunner : Signal (Task x ())
port asyncrunner = Oprocesso.ioport initmodel errorHandler

port unbatch : Signal (Task x ())
port unbatch = Oprocesso.unbatch
```

where `initmodel` is the initial model and `errorHandler` is a function of type `error -> Modifier model`, which describes how to change the model, when an error was thrown by an *asynchronous action*.

At last, there is `Oprocesso.hook : model -> Signal model`, you can use for what ever you want. Mainly:

```{.elm}
main : Signal Html
main = Signal.map view (Oprocesso.hook initmodel)
```

Here, the parameter to `Oprocesso.hook` is -- again, ... it's a flaw -- the initial model. And **it has to be the same you've used on `Oprocesso.ioport`**.


## Inner Concepts

### Actions as Lists of Operations

### Box-Port Cycles

### <a name="actionNOTmonadic"></a>Actions aren't Monadic

## Future Plans

Some questions I want to consider so far:

  - What about more complex apps, involving `fpsWhen` ?
