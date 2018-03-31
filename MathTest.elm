module MathTest exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Debug exposing (log)

-- MODEL
type alias Question =
  { x : Int
  , y : Int
  , operator : String
  , answer : Int
  , solution : Int
  , isSolutionCorrect : Bool
  }

type alias Model =
  { stars : Int
  , currentQuestion : Question
  , history : List Question
  , currentInput : String
  }

initialModel : Model
initialModel =
  { stars = 0
  , currentQuestion = Question 2 5 "+" 0 7 False
  , history = []
  , currentInput = ""
  }

-- Update
onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)

type Msg =
  Solution | Input String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Solution  ->
      log "hello"
      { model | stars = model.stars + 1}
    Input input ->
      case String.toInt(input) of
        (Ok value) -> { model | currentInput = input}
        _ -> model

-- VIEW

viewHeader : String -> Html msg
viewHeader title =
  header []
      [div [ class "siimple-box siimple-box--pink"]
           [ div [class "siimple-box-title"] [text title]
           , div [class "siimple-box-subtitle"] [text "Let's beat the math challenge!"]]
      ]

viewQuestion x operator y =
  div []
   [ text (toString x ++ operator ++ toString y ++ "=")
   , input
       [ type_ "text"
       , placeholder "What is your answer?"
       , class "siimple-input"
       , autofocus True
       , onEnter Solution
       , onInput Input]
       []
    ]


viewFooter =
  footer [class "siimple-footer"]
      [a [href "https://github.com/wickedwukong/bingo"]
         [text "View source on Github"]
      ]

showStars : Int -> Html msg
showStars stars =
  div []
      [text ("Ava, your stars: " ++ (toString stars)) ]

-- [input
--    [ type_ "text"
--    , placeholder "What is your answer?"
--    , autofocus True
--    , onInput SetNameInput]]
--
view model =
  div [class "siimple-content--fluid", align "center"]
      [ viewHeader "Hello Ava, welcome!"
      , showStars model.stars
      , viewQuestion model.currentQuestion.x model.currentQuestion.operator model.currentQuestion.y
      , div [class "debug"] [text (toString model)]
      , viewFooter
      ]

-- main =
  -- view initialModel

main =
  Html.beginnerProgram { model = initialModel
                       , view = view
                       , update = update}
