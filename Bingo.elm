module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)

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
  , alertMessage : Maybe String
}

-- palyerInfo name gameNumber =
  -- name ++ " - Game #" ++ (toString gameNumber)

-- Commands
generateRandomNumber: Cmd Msg
generateRandomNumber =
  Random.generate (\num -> NewRandom num) (Random.int 1 100)

entriesUrl : String
entriesUrl =
  "http://localhost:3000/random-entries"

getEntries : Cmd Msg
getEntries =
  (Decode.list entryDecoder)
    |> Http.get entriesUrl
    |> Http.send NewEntries
  -- Http.send (\result -> NewEntries result) (Http.getString entriesUrl)
  -- or a long handed way of doing it:
  -- Http.send NewEntries Http.getString entriesUrl)

  -- send : (Result Http.Error String -> Msg) -> Request String -> Cmd msg

-- Update
type Msg = NewGame
           | Mark Int
           | NewRandom Int
           | NewEntries (Result Http.Error (List Entry))
           | CloseAlert

-- Json Decoder
entryDecoder: Decoder Entry
entryDecoder =
  Decode.map4 Entry
    (field "id" Decode.int)
    (field "phrase" Decode.string)
    (field "points" Decode.int)
    (succeed False)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewEntries (Ok newEntries) ->
      ({model | entries = newEntries}, Cmd.none)
    NewEntries (Err error) ->
     let
        errorMessage =
          case error of
            Http.NetworkError ->
                "Is the server running?"
            Http.BadStatus response ->
                (toString response.status)
            Http.BadPayload message _ ->
                "Decoding Failed: " ++ message
            _ ->
                (toString error)
      in
          ( { model | alertMessage = Just errorMessage }, Cmd.none)
    NewRandom num ->
      ({model | gameNumber = num}, Cmd.none)
    NewGame ->
      ({model | gameNumber = model.gameNumber + 1}, getEntries)
    CloseAlert ->
      ({model | alertMessage = Nothing}, Cmd.none)
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
  { name =  "mike"
  , gameNumber =  1
  , entries =  []
  , alertMessage = Nothing
 }



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
      , viewAlertMessage model.alertMessage
      , viewEntryList model.entries
      , viewScore (sumMarkedPoints model.entries)
      , div [class "button-group"]
            [button [onClick NewGame] [text "New Game"]]
      , div [class "debug"] [text (toString model)]
      , viewFooter
      ]

viewAlertMessage: Maybe String -> Html Msg
viewAlertMessage alertMessage =
  case alertMessage of
    Just message ->
      div [ class "alert"]
          [ span [ class "close", onClick CloseAlert] [text "X"]
           ,  text message]
    Nothing ->
      text ""
-- main : Html Msg
-- main =
--   update NewGame initialModel
--   |> view


main : Program Never Model Msg
main =
  Html.program { init = (initialModel, getEntries)
                       , view = view
                       , update = update
                       , subscriptions = (\model -> Sub.none) }
