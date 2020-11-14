module Keyboard exposing (..)

import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
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


margin =
    20


borderWidth =
    2



--kb : Float -> Sem.Contract -> Element msg


kb : Float -> Element Sem.Msg
kb screenHeight =
    row
        [ alignBottom
        , width fill
        , height <| px <| round <| screenHeight * 0.5
        , Border.width borderWidth
        , Border.roundEach { corners | topLeft = margin, topRight = margin }
        , Border.color <| Hi.keyboardBorder
        , Bg.color Hi.bgBlue
        , Font.size 20
        , padding margin
        , spacing margin
        ]
        [ column [ width fill, height fill, spacing margin ]
            [ key (Paste <| Sem.ContractExpr Sem.Refund) Hi.refund "Refund"
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.string "\" \""
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.numColor "42"
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.value "ValueId"
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.value "Choose"
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.deposit "Deposit"
            ]
        , column [ width fill, height fill, spacing margin ]
            [ key (Paste <| Sem.ContractExpr Sem.Refund) Hi.pay "Pay"
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.accountId "Account"
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.notColor "Not"
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.trueObs "True"
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.falseObs "False"
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.choice "Choice"
            ]
        , column [ width fill, height fill, spacing margin ]
            [ key (Paste <| Sem.ContractExpr Sem.Refund) Hi.letColor "Let"
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.ifColor "If"
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.andOr "And"
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.valueGT ">"
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.valueLT "<"
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.notify "Notify"
            ]
        , column [ width fill, height fill, spacing margin ]
            [ key (Paste <| Sem.ContractExpr Sem.Refund) (rgb 1 0.2 0) "When"
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.caseColor "Case"
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.andOr "Or"
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.valueGE ">="
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.valueLE "<="
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.valueEQ "="
            ]
        , column [ width fill, height fill, spacing margin ]
            [ closeKeyboard
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.black ""
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.black ""
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.black ""
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.white "Copy"
            , key (Paste <| Sem.ContractExpr Sem.Refund) Hi.white "Paste\n(Type)"
            ]
        ]


closeKeyboard : Element Sem.Msg
closeKeyboard =
    Input.button
        [ Font.center
        , Font.color Hi.white
        , Bg.color <| Hi.addAlpha 0.2 Hi.black
        , Border.width borderWidth
        , Border.rounded margin
        , Border.color Hi.black
        , width fill
        , height fill
        ]
        { onPress = Just Sem.ToggleKeyboard
        , label =
            el
                [ centerY
                , centerX
                , Font.size 40
                ]
            <|
                text "✕"
        }



{-

   row [ width fill, height fill, spacing margin ]
       ]
   , row [ width fill, height fill, spacing margin ]
       ]
   , row [ width fill, height <| fillPortion 4, spacing margin ]
       [ column [ width <| fillPortion 4, height fill, spacing margin ]
           [ row [ width fill, height fill, spacing margin ]
               ]
           , row [ width fill, height fill, spacing margin ]
               ]
           , row [ width fill, height fill, spacing margin ]
               ]
           , row [ width fill, height fill, spacing margin ]
               ]
           ]
       , column
           [ height <| px 500
           , alignTop
           , width fill
           , spacing margin
           , clipY
           ]
           [ column
               [ height <| px 400
               , width fill
               , clipY
               ]
           ]
       ]
   ]

-}
-- Keyboard Button --


type KeyMsg
    = Copy Sem.Expr
    | Paste Sem.Expr


key : KeyMsg -> Color -> String -> Element msg
key keyType color label =
    el
        [ Font.center
        , Font.color color
        , Bg.color <| Hi.addAlpha 0.2 color
        , Border.width borderWidth
        , Border.rounded margin
        , width fill
        , height fill

        --, paddingXY margin (margin * 3 // 2)
        ]
    <|
        el [ centerY, centerX ] <|
            text label



-- Mini Contract View --


genMiniContractView : Sem.Contract -> Element msg
genMiniContractView contract =
    case contract of
        Sem.Refund ->
            singletonMiniHeader Hi.refund

        Sem.Pay p1 p2 v a ->
            [ topMiniHeader
            , subMiniScope <| genMiniAccountIdView
            , midMiniHeader
            , subMiniScope <| genMiniPayeeView p2
            , midMiniHeader
            , subMiniScope <| genMiniValueView v
            , midMiniHeader
            , subMiniScope <| genMiniContractView a
            ]
                |> scopeMiniBlock Hi.pay

        Sem.If o a b ->
            [ topMiniHeader
            , subMiniScope <| genMiniObservationView o
            , midMiniHeader
            , subMiniScope <| genMiniContractView a
            , midMiniHeader
            , subMiniScope <| genMiniContractView b
            ]
                |> scopeMiniBlock Hi.ifColor

        Sem.When xs _ y ->
            [ topMiniHeader
            , column
                [ paddingEach { edges | top = 1, left = 1, bottom = 1 }
                ]
                (xs |> List.map genMiniCaseView)
            , midMiniHeader
            , subMiniScope <| genMiniNumberView
            , midMiniHeader
            , subMiniScope <| genMiniContractView y
            ]
                |> scopeMiniBlock Hi.contractColor

        Sem.Let _ val a ->
            [ topMiniHeader
            , subMiniScope <| genMiniNumberView
            , midMiniHeader
            , subMiniScope <| genMiniValueView val
            , midMiniHeader
            , subMiniScope <| genMiniContractView a
            ]
                |> scopeMiniBlock Hi.letColor


genMiniCaseView : Sem.Case Sem.Action Sem.Contract -> Element msg
genMiniCaseView (Sem.Case a c) =
    [ topMiniHeader
    , subMiniScope <| genMiniActionView a
    , midMiniHeader
    , subMiniScope <| genMiniContractView c
    ]
        |> scopeMiniBlock Hi.caseColor


genMiniPayeeView : Sem.Payee -> Element msg
genMiniPayeeView payee =
    case payee of
        Sem.Account _ ->
            genMiniAccountIdView

        Sem.Party _ ->
            genMiniStringView


genMiniAccountIdView : Element msg
genMiniAccountIdView =
    [ topMiniHeader
    , subMiniScope <| genMiniNumberView
    , midMiniHeader
    , subMiniScope <| genMiniStringView
    ]
        |> scopeMiniBlock Hi.accountId


genMiniStringView : Element msg
genMiniStringView =
    singletonMiniHeader (rgb 0 0.5 0.2)


genMiniNumberView : Element msg
genMiniNumberView =
    singletonMiniHeader (rgb 0.6 0.8 0.2)


genMiniValueView : Sem.Value Sem.Observation -> Element msg
genMiniValueView val =
    case val of
        Sem.AvailableMoney ->
            singletonMiniHeader (rgb 0.6 0.8 0.2)

        Sem.Constant _ ->
            singletonMiniHeader (rgb 0.6 0.8 0.2)

        Sem.NegValue v ->
            [ topMiniHeader
            , subMiniScope <| genMiniValueView v
            ]
                |> scopeMiniBlock (rgb 0.6 0.8 0.2)

        Sem.AddValue v1 v2 ->
            [ subMiniScope <| genMiniValueView v1
            , midMiniHeader
            , subMiniScope <| genMiniValueView v2
            ]
                |> scopeMiniBlock (rgb 0.6 0.8 0.2)

        Sem.SubValue v1 v2 ->
            [ subMiniScope <| genMiniValueView v1
            , midMiniHeader
            , subMiniScope <| genMiniValueView v2
            ]
                |> scopeMiniBlock (rgb 0.6 0.8 0.2)

        Sem.MulValue v1 v2 ->
            [ subMiniScope <| genMiniValueView v1
            , midMiniHeader
            , subMiniScope <| genMiniValueView v2
            ]
                |> scopeMiniBlock (rgb 0.6 0.8 0.2)

        Sem.Scale _ v ->
            [ topMiniHeader
            , subMiniScope <| genMiniRationalView
            , midMiniHeader
            , subMiniScope <| genMiniValueView v
            ]
                |> scopeMiniBlock
                    (rgb 0.6 0.8 0.2)

        Sem.ChoiceValue id v ->
            [ topMiniHeader
            , subMiniScope <| genMiniChoiceIdView id
            , midMiniHeader
            , subMiniScope <| genMiniValueView v
            , endMiniHeader
            ]
                |> scopeMiniBlock
                    (rgb 0.6 0.8 0.2)

        Sem.SlotIntervalStart ->
            singletonMiniHeader (rgb 0.6 0.8 0.2)

        Sem.SlotIntervalEnd ->
            singletonMiniHeader (rgb 0.6 0.8 0.2)

        Sem.UseValue _ ->
            [ topMiniHeader
            , subMiniScope <| genMiniNumberView
            ]
                |> scopeMiniBlock (rgb 0.6 0.8 0.2)

        Sem.Cond o a b ->
            [ topMiniHeader
            , subMiniScope <| genMiniObservationView o
            , midMiniHeader
            , subMiniScope <| genMiniValueView a
            , midMiniHeader
            , subMiniScope <| genMiniValueView b
            ]
                |> scopeMiniBlock
                    (rgb 0.6 0.8 0.2)


genMiniObservationView : Sem.Observation -> Element msg
genMiniObservationView obs =
    case obs of
        Sem.AndObs o1 o2 ->
            [ subMiniScope <| genMiniObservationView o1
            , midMiniHeader
            , subMiniScope <| genMiniObservationView o2
            ]
                |> scopeMiniBlock (rgb 0.4 0.5 1)

        Sem.OrObs o1 o2 ->
            [ subMiniScope <| genMiniObservationView o1
            , midMiniHeader
            , subMiniScope <| genMiniObservationView o2
            ]
                |> scopeMiniBlock (rgb 0.4 0.5 1)

        Sem.NotObs o ->
            [ topMiniHeader
            , subMiniScope <| genMiniObservationView o
            ]
                |> scopeMiniBlock (rgb 0.32 0.4 0.8)

        Sem.ChooseSomething id ->
            [ topMiniHeader
            , subMiniScope <| genMiniChoiceIdView id
            ]
                |> scopeMiniBlock (rgb 0.32 0.4 0.8)

        Sem.ValueGE v1 v2 ->
            [ subMiniScope <| genMiniValueView v1
            , midMiniHeader
            , subMiniScope <| genMiniValueView v2
            ]
                |> scopeMiniBlock (rgb 0.4 0.5 1)

        Sem.ValueGT v1 v2 ->
            [ subMiniScope <| genMiniValueView v1
            , midMiniHeader
            , subMiniScope <| genMiniValueView v2
            ]
                |> scopeMiniBlock (rgb 0.4 0.5 1)

        Sem.ValueLT v1 v2 ->
            [ subMiniScope <| genMiniValueView v1
            , midMiniHeader
            , subMiniScope <| genMiniValueView v2
            ]
                |> scopeMiniBlock (rgb 0.4 0.5 1)

        Sem.ValueLE v1 v2 ->
            [ subMiniScope <| genMiniValueView v1
            , midMiniHeader
            , subMiniScope <| genMiniValueView v2
            ]
                |> scopeMiniBlock (rgb 0.4 0.5 1)

        Sem.ValueEQ v1 v2 ->
            [ subMiniScope <| genMiniValueView v1
            , midMiniHeader
            , subMiniScope <| genMiniValueView v2
            ]
                |> scopeMiniBlock (rgb 0.4 0.5 1)

        Sem.TrueObs ->
            singletonMiniHeader (rgb 0.64 0.8 1)

        Sem.FalseObs ->
            singletonMiniHeader (rgb 0.2 0.25 0.5)


genMiniActionView : Sem.Action -> Element msg
genMiniActionView action =
    case action of
        Sem.Deposit _ _ val ->
            [ topMiniHeader
            , subMiniScope <| genMiniAccountIdView
            , midMiniHeader
            , subMiniScope <| genMiniStringView
            , midMiniHeader
            , subMiniScope <| genMiniValueView val
            ]
                |> scopeMiniBlock (rgb 0.6 0.3 0.7)

        Sem.Choice _ bounds ->
            [ topMiniHeader
            , subMiniScope <| genMiniStringView
            , midMiniHeader
            , subMiniScope <| genMiniStringView
            , midMiniHeader
            , column
                [ paddingEach { edges | top = 1, left = 1, bottom = 1 }
                ]
                (bounds |> List.map genMiniBoundsView)
            ]
                |> scopeMiniBlock (rgb 0.6 0.3 0.7)

        Sem.Notify obs ->
            [ topMiniHeader
            , subMiniScope <| genMiniObservationView obs
            ]
                |> scopeMiniBlock (rgb 0.6 0.3 0.7)


genMiniBoundsView : Sem.Bound -> Element msg
genMiniBoundsView (Sem.Bound a b) =
    [ topMiniHeader
    , subMiniScope <| genMiniNumberView
    , midMiniHeader
    , subMiniScope <| genMiniNumberView
    ]
        |> scopeMiniBlock (rgb 0.6 0.3 0.15)


genMiniChoiceIdView : Sem.ChoiceId -> Element msg
genMiniChoiceIdView (Sem.ChoiceId choice owner) =
    [ topMiniHeader
    , subMiniScope <| genMiniStringView
    , midMiniHeader
    , subMiniScope <| genMiniStringView
    ]
        |> scopeMiniBlock (rgb 0.3 0.6 0.15)


genMiniRationalView : Element msg
genMiniRationalView =
    [ subMiniScope <| genMiniNumberView
    , midMiniHeader
    , subMiniScope <| genMiniNumberView
    ]
        |> scopeMiniBlock (rgb 0.6 0.8 0.2)



-- Managing Mini Complexity --


scopeMiniBlock : Color -> List (Element msg) -> Element msg
scopeMiniBlock color elems =
    column
        [ Border.widthEach { edges | left = borderWidth // 2 }
        , Border.roundEach
            { corners
                | topLeft = borderWidth
                , bottomLeft = borderWidth
            }
        , Font.color color
        ]
        elems


topMiniHeader : Element msg
topMiniHeader =
    el
        [ Border.widthEach
            { edges
                | top = borderWidth // 2
                , right = borderWidth // 2
                , bottom = borderWidth // 2
            }
        , Border.roundEach
            { corners
                | topLeft = borderWidth
                , topRight = borderWidth
                , bottomRight = borderWidth
            }
        , paddingXY borderWidth (borderWidth // 2)
        ]
    <|
        text ""


midMiniHeader : Element msg
midMiniHeader =
    el
        [ Border.widthEach
            { edges
                | top = borderWidth // 2
                , right = borderWidth // 2
                , bottom = borderWidth // 2
            }
        , Border.roundEach
            { corners
                | topRight = borderWidth
                , bottomRight = borderWidth
            }
        , paddingXY borderWidth (borderWidth // 2)
        ]
    <|
        text ""


endMiniHeader : Element msg
endMiniHeader =
    el
        [ Border.widthEach
            { edges
                | top = borderWidth // 2
                , right = borderWidth // 2
                , bottom = borderWidth // 2
            }
        , Border.roundEach
            { corners
                | bottomLeft = borderWidth
                , topRight = borderWidth
                , bottomRight = borderWidth
            }
        , paddingXY borderWidth (borderWidth // 2)
        ]
    <|
        text ""


singletonMiniHeader : Color -> Element msg
singletonMiniHeader color =
    el
        [ Font.color color
        , Border.width <| borderWidth // 2
        , Border.rounded borderWidth
        , paddingXY borderWidth (borderWidth // 2)
        ]
    <|
        text ""


subMiniScope : Element msg -> Element msg
subMiniScope elem =
    el
        [ paddingEach
            { edges
                | top = borderWidth // 2
                , left = borderWidth // 2
                , bottom = borderWidth // 2
            }
        ]
        elem
