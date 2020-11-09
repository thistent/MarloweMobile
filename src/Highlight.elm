module Highlight exposing (..)

import Element exposing (Color, rgb)


keyboardBorder =
    rgb 0.2 0.1 0.4


keyboardText =
    rgb 0.3 0.15 0.6


contractBase =
    rgb 0.65 0 0



-- Syntax Highlighting Colors --


refund =
    rgb 0.65 0.05 0


pay =
    rgb 1 0.55 0.15


contractColor : Color
contractColor =
    rgb 1 0.2 0


ifColor =
    rgb 1 0.4 0


letColor =
    rgb 0.8 0.2 0.4


caseColor =
    rgb 1 0.8 0


accountId =
    rgb 0 0.45 0.65


string =
    rgb 0 0.5 0.2


numColor =
    rgb 0.6 0.8 0.2


value =
    rgb 0.5 0.8 0.3


andOr =
    rgb 0.4 0.5 1



-- Other Colors --


black : Color
black =
    rgb 0.06 0.06 0.1


white : Color
white =
    rgb 0.9 0.95 1


bgBlue : Color
bgBlue =
    rgb 0.08 0.08 0.16


accentPink : Color
accentPink =
    rgb 1 0.7 0.95
