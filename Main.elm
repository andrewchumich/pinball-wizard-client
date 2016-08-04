import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import WebSocket
import Json.Decode exposing (Decoder, decodeString, int, string, object2, (:=))

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


echoServer : String
echoServer =
  "ws://localhost:3000"

type alias Score = 
  { score : Int
  , user : String
  }

-- MODEL

type alias Model =
  { input : String 
  , currentScore : Score
  }


init : (Model, Cmd Msg)
init =
  (Model "" (Score 0 ""), Cmd.none)



-- UPDATE


type Msg
  = NewScore String
  | Input String
  | Send


update : Msg -> Model -> (Model, Cmd Msg)
update msg {input, currentScore} =
  case msg of
    Input newInput ->
      (Model newInput currentScore, Cmd.none)

    NewScore score ->
      (Model input (decodeScore score), Cmd.none)

    Send ->
      -- pre-populate the model with the send username string
      -- the next NewScore may overwrite it
      (Model "" (Score currentScore.score input), WebSocket.send echoServer input)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen echoServer NewScore



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [onInput Input] []
    , button [onClick Send] [text "Send"]
    , div [] [viewScore model.currentScore]
    ]


viewScore : Score -> Html msg
viewScore score =
  div [] 
  [ p [] [text score.user]
  , div [] [text (toString score.score)]
  ]


-- DECODERS

scoreDecoder : Decoder Score
scoreDecoder = 
  object2 Score
    ("score" := int)
    ("user" := string)

decodeScore : String -> Score
decodeScore payload = 
  case (decodeString scoreDecoder payload) of 
    Ok val -> val
    Err message -> (Score 0 "")