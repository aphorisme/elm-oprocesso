module JsonEcho where


-- Oprocesso:
import  Oprocesso               exposing ( (>>-)
                                         , thenDo
                                         , pure
                                         , purelift
                                         , with
                                         )
import  Oprocesso.Types         exposing (..)

-- core:
import  Task                    exposing (Task)
import  Signal                  exposing (Signal, send, Address)
import  Http
import  Json.Decode             exposing (object2, string, (:=))

-- Html:
import  Html                    exposing (..)
import  Html.Events             exposing (..)
import  Html.Attributes         exposing (..)
import  Html.Lazy               exposing (lazy, lazy2)

main : Signal Html
main = Signal.map view (Oprocesso.hook initmodel)

port asyncrunner : Signal (Task x ())
port asyncrunner = Oprocesso.ioport initmodel errorHandler

port unbatch : Signal (Task x ())
port unbatch = Oprocesso.unbatch

--///////////--
--   MODEL   --
--///////////--

type alias Model =
  { entries : List String
  , typed   : String
  }

initmodel : Model
initmodel = { entries = ["Type: 'v1/the/v2/heck' to get 'the/heck' echoed."], typed = ""}

errorHandler : String -> Modifier Model
errorHandler s = addEntry <| "Error: " ++ s

------------
-- modifiers:
addEntry : String -> Modifier Model
addEntry ent =
  \m -> { m | entries <- m.entries ++ [ent] }


addTyped : Modifier Model
addTyped =
  \m -> { m | entries <- m.entries ++ ["\\> " ++ m.typed] }


setInput : String -> Modifier Model
setInput inp =
  \m -> { m | typed <- inp }

--/////////////--
--   ACTIONS   --
--/////////////--

-------------
-- pure ones:
typing : String -> Action Model String
typing = purelift setInput

--------------------------
-- dependent asynchronous:
(>>=) = Task.andThen

{-| 'makeRequest' adds the current input into the history, starts an asynchronous request based on the current input (and which pushes its result into the history when completed) and empties the input box right after. -}
makeRequest : Action Model String
makeRequest =
           addTyped
              >>- asyncRequestJson `with` .typed
  `thenDo` pure (setInput "")


--/////////--
--  TASKS  --
--/////////--

-------------
-- dependent:
asyncRequestJson : String -> AsyncModifier Model String
asyncRequestJson typd =
  let vvDecoder_ =
        object2 (,)
          ("v1" := string)
          ("v2" := string)
  in Task.mapError toString (Http.get vvDecoder_ ("http://echo.jsontest.com/" ++ typd))
     >>= \(s1, s2) -> Task.succeed <| addEntry ("Echoed: " ++ s1 ++ "/" ++ s2)


--////////--
--  VIEW  --
--////////--
view : Model -> Html
view m =
  div [ id "wrapper" ]
      [ lazy inputfield m.typed
      , lazy (\es -> div [ id "entries"] (hentries es)) m.entries ]


inputfield : String -> Html
inputfield s =
  input [ id "inputfield"
        , value s
        , on "input" (Json.Decode.map typing targetValue) (Signal.message <| .address Oprocesso.actionbox)
        , onEnter (.address Oprocesso.actionbox) makeRequest
        ]
        []

hentries : List String -> List Html
hentries es =
  case es of
    []       -> []
    (x::xs)  -> div [ class "entry" ] [ span [] [text x] ] :: hentries xs


--////////--
--  MISC  --
--////////--

onEnter : Address a -> a -> Attribute
onEnter address value =
    on "keydown"
      (Json.Decode.customDecoder keyCode is13)
      (\_ -> Signal.message address value)


is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"
