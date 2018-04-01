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

type alias BasicQuestion =
  { x : Int
  , y : Int
  , operator : String
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
toOperator : Int -> String
toOperator i =
  case i of
    1 -> "x"
    2 -> "+"
    3 -> "-"
    otherWise -> Debug.crash "Out of range"

randomOperatorGenerator : Random.Generator String
randomOperatorGenerator =
    int 1 3 |> Random.map toOperator

generateRandomOperator : Cmd Msg
generateRandomOperator =
  Random.generate NewRandomOperator randomOperatorGenerator

generateRandomNumbers : Cmd Msg
generateRandomNumbers =
    Random.generate NewRandom (Random.list 2 (Random.int 0 100))

under100Generator : String -> Generator BasicQuestion
under100Generator operator =
   Random.map2
       (\x y -> BasicQuestion x y operator)
       (Random.int 0 100)
       (Random.int 0 100)

under10Generator : String -> Generator BasicQuestion
under10Generator operator =
   Random.map2
       (\x y -> BasicQuestion x y operator)
       (Random.int 0 10)
       (Random.int 0 10)

newGenerateRandomNumbers : String -> Cmd Msg
newGenerateRandomNumbers operator =
  if (operator == "+") then
       Random.generate NewNewRandom (under100Generator "+")
  else if (operator == "x") then
       Random.generate NewNewRandom (under10Generator "x")
  else
       Random.generate NewNewRandom (under100Generator "-")



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
  Solution | Input String | NewRandom (List Int) | NewRandomOperator String | NewNewRandom BasicQuestion

makeQuestion : List Int -> Question
makeQuestion values =
  case values of
    x :: y :: _ -> Question x y "+" 0 (x + y) False
    _ -> Question 0 0 "+" 0 0 False

newMakeQuestion : BasicQuestion -> Question
newMakeQuestion bq =
  if (bq.operator == "+") then
    Question bq.x bq.y bq.operator -99999999 (bq.x + bq.y) False
  else if (bq.operator == "x") then
    Question bq.x bq.y bq.operator -99999999 (bq.x * bq.y) False
  else if (bq.x > bq.y) then
    Question bq.x bq.y bq.operator -99999999 (bq.x - bq.y) False
  else
    Question bq.y bq.x bq.operator -99999999 (bq.y - bq.x) False

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewRandomOperator operator ->
      (model, newGenerateRandomNumbers operator)
    NewNewRandom basicQuestion ->
      let newQuestion = newMakeQuestion basicQuestion
      in  ({model | currentQuestion = newQuestion}, Cmd.none)
    NewRandom values ->
      let newQuestion = makeQuestion values
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
            if (isSolutionCorrect) then
              ({model | stars = model.stars + 1, history = newCurrentQuestion :: model.history, currentInput = ""}, generateRandomOperator)
            else
              ({model | history = newCurrentQuestion :: model.history, currentInput = ""}, generateRandomOperator)
        _ -> (model, Cmd.none)
    Input input ->
        ({model | currentInput = input}, Cmd.none)

-- VIEW
showSolutionIfAnswerIsWrong : Question -> Html Msg
showSolutionIfAnswerIsWrong q =
  if (q.isSolutionCorrect) then
    text ""
  else
    text ("( ==> " ++ toString q.x ++ q.operator ++ toString q.y ++ "=" ++ toString q.solution ++ ")")

showFeedback : Question -> Html Msg
showFeedback q =
  if (q.isSolutionCorrect) then
    img [src "./happy.png", height 15, width 15] []
  else
    img [src "./sad.png", height 15, width 15] []

viewQuestionItem: Question -> Html Msg
viewQuestionItem q =
  div [ class "siimple-table-row"]
       [ div [class "siimple-table-cell"] [text (toString q.x ++ q.operator ++ toString q.y ++ "=" ++ toString q.answer)]
       , div [class "siimple-table-cell"] [showFeedback q, showSolutionIfAnswerIsWrong q]
       ]
  -- li []
  --  [ span [] [text (toString q.x ++ q.operator ++ toString q.y ++ "=" ++ toString q.answer)]
  --  , span [] [showFeedback q]
  --  ]
  --
viewHistory : List Question -> Html Msg
viewHistory questions =
  let
     listOfQuestions =
       List.map viewQuestionItem questions
  in
  div [class "siimple-table"]
       [div [class "siimple-table-body siimple-table--border"] listOfQuestions]
  -- ul [] listOfQuestions



viewHeader : String -> Html msg
viewHeader title =
  header []
      [div [ class "siimple-box siimple-box--teal"]
           [ div [class "siimple-box-title"] [text title]
           , div [class "siimple-box-subtitle"] [text "Let's beat the math challenge!"]]
      ]

viewQuestion x operator y currentInput =
  div [class "siimple-h4"]
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
        [ span [] [text ("Stars: " ++ toString stars)]
        , div []
               (List.map (\x -> img [src "./star.jpeg", height 23, width 23] []) (List.range 1 stars))
         ]


-- [input
--    [ type_ "text"
--    , placeholder "What is your answer?"
--    , autofocus True
--    , onInput SetNameInput]]
--
view model =
  div [class "siimple-content--fluid", align "center"]
      [ viewHeader "Hello Budding Mathematician, welcome!"
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
        { init = (initialModel, generateRandomOperator )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none )
        }
