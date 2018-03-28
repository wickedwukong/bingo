module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

type alias Entry =
  { id : Int
  , phrase : String
  , points : Int
  , marked : Bool
  }

type alias Model =
  { name : String
  , gameNumber : Int
  , entries : List Entry
}

-- palyerInfo name gameNumber =
  -- name ++ " - Game #" ++ (toString gameNumber)

-- MODEL
initialModel : Model
initialModel =
  {name =  "mike"
  ,gameNumber =  3
  ,entries =  initialEntries
 }


initialEntries : List Entry
initialEntries =
  [ Entry 1 "Future proof" 100 False
  , Entry 2 "Doing Agile" 200 False
  , Entry 3 "In the cloud" 300 False
  , Entry 4 "Rock star Ninja" 300 False
  ]


playerInfo : String -> Int -> String
playerInfo =
  \name gameNumber -> name ++ " - Game #" ++ (toString gameNumber)

viewPlayer : String -> Int -> Html msg
viewPlayer name gameNumber =
  let
      playerInfoText =
          playerInfo name gameNumber
            |> String.toUpper
            |> text
  in
      h2 [id "info", class "classy"]
          [playerInfoText]

viewHeader : String -> Html msg
viewHeader title =
  header []
      [h1 [] [text title]]

viewFooter : Html msg
viewFooter =
  footer []
      [a [href "http://elm-lang.org"]
         [text "Powered by Elm"]
      ]

viewEntryItem: Entry -> Html msg
viewEntryItem entry =
  li []
   [ span [class "phrase"] [text entry.phrase]
   , span [class "points"] [text (toString entry.points)]
   ]

viewEntryList : List Entry -> Html msg
viewEntryList entries =
  let
     listOfEntries =
       List.map viewEntryItem entries
  in
  ul [] listOfEntries

view : Model -> Html msg
view model =
  div [class "content"]
      [ viewHeader "BUZZWORD BINGO"
      , viewPlayer model.name model.gameNumber
      , viewEntryList model.entries
      , div [class "debug"] [text (toString model)]
      , viewFooter
      ]

main : Html msg
main =
  view initialModel
