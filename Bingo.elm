module Bingo exposing (..)

import Html

-- palyerInfo name gameNumber =
  -- name ++ " - Game #" ++ (toString gameNumber)

palyerInfo =
  \name gameNumber -> name ++ " - Game #" ++ (toString gameNumber)

palyerInfoText name gameNumber =
  palyerInfo name gameNumber
  |> String.toUpper
  |> Html.text

main =
  palyerInfoText "Xuemin" 20
