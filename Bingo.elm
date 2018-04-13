module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random

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

-- Commands
generateRandomNumber: Cmd Msg
generateRandomNumber =
  Random.generate (\num -> NewRandom num) (Random.int 1 100)

-- Update
type Msg = NewGame | Mark Int | NewRandom Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewRandom num ->
      ({model | gameNumber = num}, Cmd.none)
    NewGame ->
      ({model | entries = initialEntries}, generateRandomNumber)
    Mark id ->
      let
        markEntry e =
          if (e.id == id) then
            {e | marked = (not e.marked)}
          else
            e
      in ({model | entries = List.map markEntry model.entries}, Cmd.none)

-- MODEL
initialModel : Model
initialModel =
  {name =  "mike"
  ,gameNumber =  1
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

viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber =
  let
      playerInfoText =
          playerInfo name gameNumber
            |> String.toUpper
            |> text
  in
      h2 [id "info", class "classy"]
          [playerInfoText]

viewHeader : String -> Html Msg
viewHeader title =
  header []
      [h1 [] [text title]]

viewFooter : Html Msg
viewFooter =
  footer []
      [a [href "http://elm-lang.org"]
         [text "Powered by Elm"]
      ]

viewEntryItem: Entry -> Html Msg
viewEntryItem entry =
  li [ classList [("marked", entry.marked)], onClick (Mark entry.id)]
   [ span [class "phrase"] [text entry.phrase]
   , span [class "points"] [text (toString entry.points)]
   ]

viewEntryList : List Entry -> Html Msg
viewEntryList entries =
  let
     listOfEntries =
       List.map viewEntryItem entries
  in
  ul [] listOfEntries

sumMarkedPoints: List Entry -> Int
sumMarkedPoints entries =
  entries
    |> List.filter .marked
    |> List.map .points
    |> List.sum

viewScore: Int -> Html Msg
viewScore sum =
  div
     [ class "score"]
     [ span [class "label"] [text "Score"]
     , span [class "value"] [text (toString sum)]
     ]

view : Model -> Html Msg
view model =
  div [class "content"]
      [ viewHeader "BUZZWORD BINGO"
      , viewPlayer model.name model.gameNumber
      , viewEntryList model.entries
      , viewScore (sumMarkedPoints model.entries)
      , div [class "button-group"]
            [button [onClick NewGame] [text "New Game"]]
      , div [class "debug"] [text (toString model)]
      , viewFooter
      ]

-- main : Html Msg
-- main =
--   update NewGame initialModel
--   |> view


main : Program Never Model Msg
main =
  Html.program { init = (initialModel, generateRandomNumber)
                       , view = view
                       , update = update
                       , subscriptions = (\model -> Sub.none) }
