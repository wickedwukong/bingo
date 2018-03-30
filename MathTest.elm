module MathTest exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

-- MODEL

question =
  { x = 1
  , y = 1
  , operator = "+"
  , points = 0
  }

type alias Question =
  { x : Int
  , y : Int
  , operator : String
  , answer : Int
  , solution : Int
  }

type alias Model =
  { stars : Int
  , currentQuestion : Question
  , history : List Question
  }

initialModel : Model
initialModel =
  { stars = 0
  , currentQuestion = Question 2 5 "+" 0 7
  , history = []
  }

-- VIEW

viewHeader : String -> Html msg
viewHeader title =
  header []
      [div [ class "siimple-box siimple-box--pink"]
           [ div [class "siimple-box-title"] [text title]
           , div [class "siimple-box-subtitle"] [text "Let's beat the math challenge!"]]
      ]

viewQuestion x operator y =
  div [class "classy"]
   [(text (toString x ++ operator ++ toString y ++ "=")), (input [type_ "text", placeholder "What is your answer?", class "siimple-input"] [])]


viewFooter =
  footer []
      [a [href "http://elm-lang.org"]
         [text "Powered by Elm"]
      ]

showStars : Int -> Html msg
showStars point =
  div []
      [text "Ava, your stars: ***" ]

answer =
   div []
    [ input [ type_ "text", placeholder "What is your answer?"] []]

-- [input
--    [ type_ "text"
--    , placeholder "What is your answer?"
--    , autofocus True
--    , onInput SetNameInput]]
--
view model =
  div [class "siimple-content--medium"]
      [ viewHeader "Hello Ava, welcome!"
      , showStars model.stars
      , viewQuestion model.currentQuestion.x model.currentQuestion.operator model.currentQuestion.y
      , answer
      , div [class "debug"] [text (toString model)]
      , viewFooter
      ]

main =
  view initialModel
