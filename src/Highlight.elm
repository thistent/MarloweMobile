module Highlight exposing
    ( Blink(..)
    , Theme
    , addAlpha
    , blinkColor
    , keyboardBorder
    , lightTheme
    , lighten
    )

import Color as C
import Color.Manipulate as CM
import Element exposing (Color, fromRgb, rgb, toRgb)


type alias Theme =
    { contract : Color
    , observation : Color
    , action : Color
    , caseColor : Color
    , value : Color
    , payee : Color
    , party : Color
    , token : Color
    , bound : Color

    --, str : Color
    --, num : Color
    , accent : Color
    , bg : Color
    , barBg : Color
    , fg : Color
    }


lightTheme : Theme
lightTheme =
    { contract = rgb 0.75 0.25 1
    , observation = rgb 0.1 0.8 0.7
    , action = rgb 1 0.75 0.25
    , caseColor = rgb 1 0.5 0.25
    , value = rgb 1 0 0.25
    , payee = rgb 0.5 0.25 1
    , party = rgb 1 0.5 0.5
    , token = rgb 1 0.25 0
    , bound = rgb 0 0.5 0.5

    --, str = rgb 0.25 0.75 0
    --, num = rgb 1 0.5 0
    , accent = accentPink
    , bg = black
    , barBg = bgBlue
    , fg = white
    }


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
    rgb 0.1 0.1 0.2


accentPink : Color
accentPink =
    rgb 1 0.7 0.95


notColor : Color
notColor =
    rgb 0.32 0.4 0.8


chooseSomething : Color
chooseSomething =
    rgb 0.32 0.4 0.8


valueGE : Color
valueGE =
    rgb 0.4 0.5 1


valueGT : Color
valueGT =
    rgb 0.4 0.5 1


valueLT : Color
valueLT =
    rgb 0.4 0.5 1


valueLE : Color
valueLE =
    rgb 0.4 0.5 1


valueEQ : Color
valueEQ =
    rgb 0.4 0.5 1


trueObs : Color
trueObs =
    rgb 0.6 0.8 1


falseObs : Color
falseObs =
    rgb 0.45 0.6 0.75


deposit : Color
deposit =
    rgb 0.6 0.3 0.7


choice : Color
choice =
    rgb 0.6 0.3 0.7


notify : Color
notify =
    rgb 0.6 0.3 0.7


bounds : Color
bounds =
    rgb 0.6 0.3 0.15


choiceId : Color
choiceId =
    rgb 0.3 0.6 0.15


rational : Color
rational =
    rgb 0.6 0.8 0.2


token : Color
token =
    rgb 1 0.3 0



-- Color Functions --


addAlpha : Float -> Color -> Color
addAlpha alpha color =
    let
        addA c =
            { c | alpha = alpha }
    in
    color |> toRgb |> addA |> fromRgb



-- Blink Type and Color Modification --


type Blink
    = Empty
    | HalfFull
    | Full
    | HalfEmpty


nextBlink : Blink -> Blink
nextBlink blink =
    case blink of
        Empty ->
            HalfFull

        HalfFull ->
            Full

        Full ->
            HalfEmpty

        HalfEmpty ->
            Empty


lighten : Color -> Color
lighten =
    toRgb
        >> C.fromRgba
        >> CM.desaturate 0.1
        >> CM.lighten 0.1
        >> C.toRgba
        >> fromRgb


blinkColor : Blink -> Color -> Color
blinkColor blink baseColor =
    let
        half =
            baseColor
                |> toRgb
                >> C.fromRgba
                |> CM.desaturate 0.2
                >> CM.lighten 0.2
                |> C.toRgba
                >> fromRgb
    in
    case blink of
        Empty ->
            baseColor

        HalfFull ->
            half

        Full ->
            baseColor
                |> toRgb
                >> C.fromRgba
                |> CM.desaturate 0.3
                >> CM.lighten 0.4
                |> C.toRgba
                >> fromRgb

        HalfEmpty ->
            half
