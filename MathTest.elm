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


-- VIEW

viewHeader : String -> Html msg
viewHeader title =
  header []
      [h1 [] [text title]]

viewQuestion x operator y =
  h2 [class "classy"]
   [text (toString x ++ operator ++ toString y ++ "=???")]


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

view model =
  div [class "content"]
      [ viewHeader "Hello Ava! Welcome! Let's win the math challege!"
      , showStars model.points
      , viewQuestion model.x model.operator model.y
      , answer
      , div [class "debug"] [text (toString model)]
      , viewFooter
      ]

main =
  view question
