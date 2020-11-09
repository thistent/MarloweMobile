module Keyboard exposing (..)

import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Highlight as Hi
import Marlowe.Semantics as Sem


edges =
    { top = 0
    , bottom = 0
    , left = 0
    , right = 0
    }


corners =
    { topLeft = 0
    , topRight = 0
    , bottomLeft = 0
    , bottomRight = 0
    }


keyBg : Float -> Color -> Color
keyBg alpha color =
    let
        addAlpha c =
            { c | alpha = alpha }
    in
    color |> toRgb |> addAlpha |> fromRgb


margin =
    20


borderWidth =
    2


kb : Element msg
kb =
    row
        [ alignBottom
        , width fill
        , Border.width borderWidth
        , Border.roundEach { corners | topLeft = margin, topRight = margin }
        , Border.color <| Hi.keyboardBorder
        , Bg.color <| keyBg 0.9 Hi.bgBlue
        , Font.size 20
        , padding margin
        , spacing margin
        ]
        [ column [ width <| fillPortion 4, spacing margin ]
            [ row [ width fill, spacing margin ]
                [ key (ContractKey Sem.Refund) Hi.refund "Refund"
                , key (ContractKey Sem.Refund) Hi.pay "Pay"
                , key (ContractKey Sem.Refund) Hi.ifColor "If"
                , key (ContractKey Sem.Refund) (rgb 1 0.2 0) "When"
                ]
            , row [ width fill, spacing margin ]
                [ key (ContractKey Sem.Refund) Hi.letColor "Let"
                , key (ContractKey Sem.Refund) Hi.caseColor "Case"
                , key (ContractKey Sem.Refund) Hi.accountId "Account"
                , key (ContractKey Sem.Refund) Hi.string "\" \""
                ]
            , row [ width fill, spacing margin ]
                [ key (ContractKey Sem.Refund) Hi.numColor "42"
                , key (ContractKey Sem.Refund) Hi.value "ValueId"
                , key (ContractKey Sem.Refund) Hi.andOr "And"
                , key (ContractKey Sem.Refund) Hi.andOr "Or"
                ]
            ]
        , column [ Font.size 20, width fill, spacing margin ]
            [ key (ContractKey Sem.Refund) Hi.white "Copy"
            , key (ContractKey Sem.Refund) Hi.white "Paste"
            ]
        ]



-- Keyboard Button --


type KeyType
    = StringKey String
    | NumKey Int
    | TokenKey Sem.Token
    | ValueIdKey Sem.ValueId
    | PayeeKey Sem.Payee
    | BoundKey Sem.Bound
    | CaseKey (Sem.Case Sem.Action Sem.Contract)
    | RationalKey Sem.Rational
    | AccountIdKey Sem.AccountId
    | ChoiceIdKey Sem.ChoiceId
    | ContractKey Sem.Contract
    | ValueKey (Sem.Value Sem.Observation)
    | ObservationKey Sem.Observation
    | ActionKey Sem.Action


key : KeyType -> Color -> String -> Element msg
key keyType color label =
    el
        [ Font.center
        , Font.color color
        , Bg.color <| keyBg 0.2 color
        , Border.width borderWidth
        , Border.rounded margin
        , width fill
        , paddingXY margin (margin * 3 // 2)
        ]
    <|
        el [ centerY, centerX ] <|
            text label



-- Old Stuff --


kbOld : Element msg
kbOld =
    column
        [ alignBottom
        , alignRight
        , Border.width borderWidth
        , Border.rounded margin
        , Border.color <| Hi.keyboardBorder
        , Bg.color Hi.bgBlue
        , paddingEach { edges | top = margin, bottom = margin }
        , spacing <| margin
        ]
        [ column
            [ width fill
            , spacing <| margin
            , paddingXY 0 margin
            , Font.size 30
            , Font.color <| Hi.keyboardText
            , Border.widthEach { edges | bottom = borderWidth }
            , Border.dotted
            ]
            [ el [ centerX, Font.letterSpacing 10 ] <|
                text "Keyboard"
            , el [ centerX, Font.size 18, Font.letterSpacing 1.5 ] <|
                text "(Click outside to close)"
            ]
        , column
            [ width fill
            , height fill
            , spacing margin
            , padding margin
            ]
            [ row [ width fill, spacing margin ]
                [ el
                    [ padding margin
                    , width fill
                    , Bg.color <| rgb 1 0 0
                    ]
                  <|
                    text "hi"
                , el
                    [ padding margin
                    , width fill
                    , Bg.color <| rgb 0 1 0
                    ]
                  <|
                    text "hi"
                ]
            , row [ width fill, spacing margin ]
                [ el
                    [ padding margin
                    , width fill
                    , Bg.color <| rgb 1 0 0
                    ]
                  <|
                    text "hi"
                , el
                    [ padding margin
                    , width fill
                    , Bg.color <| rgb 0 1 0
                    ]
                  <|
                    text "hi"
                ]
            ]
        ]



{- column
       [ width fill
       , height fill
       , spacing margin
       ]
       [ el
           [ width fill
           , height <| fillPortion 11
           ]
         <|
           el
               [ width fill
               , height fill
               , inFront <|
                   el
                       [ width fill
                       , height fill
                       , Bg.color <| rgba 0.08 0.08 0.16 0.75
                       ]
                       none
               ]
               ([ subScope <|
                   el
                       [ width fill
                       , height <| px 200
                       , scrollbarY
                       ]
                   <|
                       genMiniContractView Regular model.sampleContract
                , endHeader "Paste"
                ]
                   |> scopeBlock Regular white
               )
       , el [ width fill, height fill ] <|
           keyboardButton Regular white "Copy"
       ]
   , column
       [ width fill
       , height fill
       , spacing margin
       ]
       [ keyboardButton Regular contractColor "All"
       , keyboardButton Regular contractColor "Const."
       , keyboardButton Regular contractColor "Neg"
       , keyboardButton Regular contractColor "Add"
       , keyboardButton Regular contractColor "Sub"
       , keyboardButton Regular contractColor "Mult"
       , keyboardButton Regular contractColor "Scale"
       , keyboardButton Regular contractColor "Choice"
       , keyboardButton Regular contractColor "Start"
       , keyboardButton Regular contractColor "End"
       , keyboardButton Regular contractColor "Use"
       , keyboardButton Regular contractColor "Cond"
       ]
       ]
-}
{- [ column [ width fill, height fill ]
       [ el
           [ width fill
           , height <| fillPortion 10
           , inFront <|
               el
                   [ width fill
                   , height fill
                   , Bg.color <| rgba 0.08 0.08 0.16 0.75
                   ]
                   none
           ]
           ([ subScope <|
               el
                   [ width fill
                   , height fill
                   , scrollbarY
                   ]
               <|
                   genMiniContractView Regular model.sampleContract
            , endHeader "Paste"
            ]
               |> scopeBlock Regular white
           )
       ]
   , column
       [ width fill
       , spacing <| margin * 2
       ]
       ]
-}
{- el [ padding margin, width fill ] <|
       column [ centerX, Font.color <| Hi.keyboardText ]
           [ el
               [ centerX

               --, Font.size 35
               --, Font.letterSpacing 5
               ]
             <|
               text "Keyboard"
           , el [] <| text "(Click outside to close)"
           ]
   , row
       [ width fill
       , height fill
       , spacing <| 2 * margin
       , paddingEach
           { edges
               | left = margin
               , right = margin
               , bottom = 2 * margin
               , top = margin
           }
       ]
       [ column
           [ alignBottom
           , spacing <| 2 * margin
           , width fill
           ]
           , keyboardButton Regular white "Copy"
           ]
       , column
           [ alignBottom, spacing <| margin * 2 ]
           [ keyboardButton Regular contractColor "All"
           , keyboardButton Regular contractColor "Const."
           , keyboardButton Regular contractColor "Neg"
           , keyboardButton Regular contractColor "Add"
           , keyboardButton Regular contractColor "Sub"
           , keyboardButton Regular contractColor "Mult"
           , keyboardButton Regular contractColor "Scale"
           , keyboardButton Regular contractColor "Choice"
           , keyboardButton Regular contractColor "Start"
           , keyboardButton Regular contractColor "End"
           , keyboardButton Regular contractColor "Use"
           , keyboardButton Regular contractColor "Cond"
           ]

       {- [ keyboardButton Regular contractColor "Refund"
          , keyboardButton Regular contractColor "Pay"
          , keyboardButton Regular contractColor "If"
          , keyboardButton Regular contractColor "When"
          , keyboardButton Regular contractColor "Let"
          ]
       -}
       ]
       ]
-}
