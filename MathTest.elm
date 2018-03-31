module MathTest exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Random exposing (..)
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
generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate NewRandom (Random.int 1 100)

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
  Solution | Input String | NewRandom Int

makeQuestion : Int -> Question
makeQuestion x =
  Question x 5 "+" 0 (x + 5) False

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewRandom value ->
      let newQuestion = makeQuestion value
      in  ({model | currentQuestion = newQuestion}, Cmd.none)
    Solution ->
      case String.toInt(model.currentInput) of
        (Ok answer) ->
          let
            oldCurrentQuestion = model.currentQuestion
            solution = oldCurrentQuestion.solution
            isSolutionCorrect = answer == solution
            newCurrentQuestion = {oldCurrentQuestion | answer = answer, isSolutionCorrect = isSolutionCorrect}
          in
            ({model | stars = model.stars + 1, history = newCurrentQuestion :: model.history, currentInput = ""}, generateRandomNumber)
        _ -> (model, Cmd.none)
    Input input ->
        ({model | currentInput = input}, Cmd.none)

-- VIEW


viewQuestionItem: Question -> Html Msg
viewQuestionItem q =
  li []
   [ span [] [text (toString q.x ++ q.operator ++ toString q.y ++ "=" ++ toString q.answer)]
   , span [] [text (toString q.isSolutionCorrect)]
   ]

viewHistory : List Question -> Html Msg
viewHistory questions =
  let
     listOfQuestions =
       List.map viewQuestionItem questions
  in
  ul [] listOfQuestions



viewHeader : String -> Html msg
viewHeader title =
  header []
      [div [ class "siimple-box siimple-box--pink"]
           [ div [class "siimple-box-title"] [text title]
           , div [class "siimple-box-subtitle"] [text "Let's beat the math challenge!"]]
      ]

viewQuestion x operator y currentInput =
  div []
   [ text (toString x ++ operator ++ toString y ++ "=")
   , input
       [ type_ "text"
       , placeholder "What is your answer?"
       , class "siimple-input"
       , value currentInput
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
      , viewQuestion model.currentQuestion.x model.currentQuestion.operator model.currentQuestion.y model.currentInput
      -- , div [class "debug"] [text (toString model)]
      , hr [] []
      , viewHistory model.history
      , viewFooter
      ]

-- main =
  -- view initialModel

main : Program Never Model Msg
main =
    Html.program
        { init = (initialModel, generateRandomNumber )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none )
        }
