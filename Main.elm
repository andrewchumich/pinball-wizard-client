import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import WebSocket
import Debug exposing (..)
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
  "ws://localhost:3000/live"

type alias Score = 
  { score : Int
  , user : String
  }

-- MODEL

type alias Model =
  { userInput : String 
  , currentScore : Score
  }


init : (Model, Cmd Msg)
init =
  (Model "" (Score 0 ""), Cmd.none)



-- UPDATE


type Msg
  = NewScore String
  | UserInput String
  | SetUser


update : Msg -> Model -> (Model, Cmd Msg)
update msg {userInput, currentScore} =
  case msg of
    UserInput newUserInput ->
      (Model newUserInput currentScore, Cmd.none)

    NewScore score ->
      (Model userInput (decodeScore score), Cmd.none)

    SetUser ->
      (Model userInput (Score currentScore.score userInput), WebSocket.send echoServer userInput)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen echoServer NewScore



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [onInput UserInput, value model.userInput] []
    , button [onClick SetUser] [text "Set User"]
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