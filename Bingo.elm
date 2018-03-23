module Bingo exposing (..)

import Html

main =
  "Hello Xuemin"
      |> String.toUpper
      |> String.repeat 3
      |> String.pad 100 '*' 
      |> Html.text
