module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Browser.Navigation as Nav
import Color as C
import Color.Manipulate as CM
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Events as ElEvent
import Element.Font as Font
import Element.Input as Input
import Highlight as Hi
import Keyboard as Kb
import Marlowe.Semantics as Sem
import Task
import Url



-- Main Function --


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = subs
        , update = update
        , view = view

        -- Stuff --
        , onUrlChange = NewUrl
        , onUrlRequest = Link
        }



-- Model & Types --


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , timeDelta : Float
    , blink : Blink
    , sampleContract : Sem.Contract
    , keyboardState : Bool
    , viewport : Dom.Viewport
    }


type Msg
    = NewUrl Url.Url
    | Link Browser.UrlRequest
    | TimeDelta Float
    | SwitchContract SampleContract
    | ToggleKeyboard
    | UpdateViewport Dom.Viewport


type SampleContract
    = CouponBond
    | Escrow
    | Swap
    | ZeroCoupon
    | NilContract


type Blink
    = Regular
    | Half
    | Full



-- Initialization --


dummyViewport : Dom.Viewport
dummyViewport =
    { scene =
        { width = 0
        , height = 0
        }
    , viewport =
        { x = 0
        , y = 0
        , width = 0
        , height = 0
        }
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    ( Model key url 0.0 Regular Sem.escrow False dummyViewport
    , Cmd.batch
        [ Task.perform UpdateViewport Dom.getViewport
        ]
    )



-- Subscriptions --


subs : Model -> Sub Msg
subs model =
    Events.onAnimationFrameDelta TimeDelta



-- Update Function --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Link _ ->
            ( model, Cmd.none )

        NewUrl _ ->
            ( model, Cmd.none )

        TimeDelta delta ->
            let
                newDelta =
                    model.timeDelta + delta
            in
            if newDelta <= 800 then
                ( { model
                    | blink =
                        if newDelta <= 200 then
                            Regular

                        else if newDelta <= 400 then
                            Half

                        else if newDelta <= 600 then
                            Full

                        else
                            Half
                    , timeDelta = model.timeDelta + delta
                  }
                , Cmd.none
                )

            else
                ( { model | timeDelta = 0 }
                , Cmd.batch
                    [ Task.perform UpdateViewport Dom.getViewport
                    ]
                )

        SwitchContract contract ->
            case contract of
                CouponBond ->
                    ( { model | sampleContract = Sem.couponBondGuaranteed }
                    , Cmd.none
                    )

                Escrow ->
                    ( { model | sampleContract = Sem.escrow }
                    , Cmd.none
                    )

                Swap ->
                    ( { model | sampleContract = Sem.swap }
                    , Cmd.none
                    )

                ZeroCoupon ->
                    ( { model | sampleContract = Sem.zeroCouponBond }
                    , Cmd.none
                    )

                NilContract ->
                    ( { model | sampleContract = Sem.Refund }
                    , Cmd.none
                    )

        ToggleKeyboard ->
            ( { model | keyboardState = not model.keyboardState }
            , Cmd.none
            )

        UpdateViewport vp ->
            ( { model | viewport = vp }, Cmd.none )



-- View Function --


view : Model -> Browser.Document Msg
view model =
    { title = "Marlowe Mobile"
    , body =
        [ Element.layout
            [ width fill
            , height fill
            , Bg.color Hi.bgBlue
            , Font.color Hi.accentPink
            , Font.size 30
            , Font.family
                [ Font.typeface "Helvetica"
                , Font.sansSerif
                ]
            , clip
            , scrollbarY
            , inFront <|
                if model.keyboardState then
                    Kb.kb model.viewport.viewport.height model.sampleContract

                else
                    none
            ]
          <|
            column
                [ width fill
                , height fill
                ]
                [ row
                    [ Font.size 60
                    , Font.letterSpacing 1.5
                    , Font.color Hi.white
                    , width fill
                    , paddingEach
                        { edges
                            | top = 2 * margin
                            , left = 2 * margin
                        }
                    ]
                    [ el [ Font.bold ] <|
                        text "Marlowe "
                    , el [] <|
                        text "Mobile"
                    ]
                , row
                    [ Border.widthEach { edges | bottom = borderWidth }
                    , Border.dotted
                    , Font.size 20
                    , paddingXY (2 * margin) margin
                    , spacing <| 2 * margin
                    , width fill
                    ]
                    [ button CouponBond "CouponBondGuaranteed"
                    , button Escrow "Escrow"
                    , button Swap "Swap"
                    , button ZeroCoupon "ZeroCouponBond"
                    , button NilContract "Nil"
                    ]
                , el
                    [ width fill
                    , height fill
                    , padding <| 2 * margin
                    , Bg.color Hi.black
                    , ElEvent.onClick ToggleKeyboard

                    --, Font.size 30
                    ]
                  <|
                    ([ topHeader "Contract such that"

                     -- TODO: model.blink along with the unique id of the subtree where blink should
                     -- start if something is selected should be passed to genContractView.
                     , subScope <| genContractView Regular model.sampleContract
                     ]
                        |> scopeBlock Regular Hi.contractBase
                    )
                ]
        ]
    }


margin : Int
margin =
    10


gap : Int
gap =
    6


borderWidth : Int
borderWidth =
    2


button : SampleContract -> String -> Element Msg
button c s =
    Input.button
        []
        { onPress = Just (SwitchContract c)
        , label = text s
        }


blinkColor : Blink -> Color -> Color
blinkColor blink baseColor =
    case blink of
        Regular ->
            baseColor

        Half ->
            baseColor
                |> toRgb
                >> C.fromRgba
                |> CM.desaturate 0.5
                >> CM.lighten 0.2
                |> C.toRgba
                >> fromRgb

        Full ->
            baseColor
                |> toRgb
                >> C.fromRgba
                |> CM.grayscale
                >> CM.lighten 0.4
                |> C.toRgba
                >> fromRgb


edges =
    { top = 0
    , bottom = 0
    , right = 0
    , left = 0
    }


corners =
    { topLeft = 0
    , topRight = 0
    , bottomLeft = 0
    , bottomRight = 0
    }



-- Marlow View Components --


genContractView : Blink -> Sem.Contract -> Element Msg
genContractView blink contract =
    case contract of
        Sem.Refund ->
            singletonHeader blink Hi.refund "Refund remaining"

        Sem.Pay p1 p2 v a ->
            [ topHeader "Pay from"
            , subScope <| genAccountIdView blink p1
            , midHeader "to"
            , subScope <| genPayeeView blink p2
            , midHeader "the amount of"
            , subScope <| genValueView blink v
            , midHeader "and continue with"
            , subScope <| genContractView blink a
            ]
                |> scopeBlock blink Hi.pay

        Sem.If o a b ->
            [ topHeader "If"
            , subScope <| genObservationView blink o
            , midHeader "then"
            , subScope <| genContractView blink a
            , midHeader "else"
            , subScope <| genContractView blink b
            ]
                |> scopeBlock blink Hi.ifColor

        Sem.When cases t y ->
            [ topHeader "When"
            , column
                [ paddingEach { edges | top = margin, left = margin, bottom = margin }
                ]
                (cases |> List.map (genCaseView blink))
            , subScope <| singletonHeader blink Hi.caseColor "..."
            , midHeader "after slot"
            , subScope <| genNumberView blink t
            , midHeader "continue as"
            , subScope <| genContractView blink y
            ]
                |> scopeBlock blink Hi.contractColor

        Sem.Let id val a ->
            [ topHeader "Let the number"
            , subScope <| genNumberView blink id
            , midHeader "identify the value"
            , subScope <| genValueView blink val
            , midHeader "in"
            , subScope <| genContractView blink a
            ]
                |> scopeBlock blink Hi.letColor


genCaseView : Blink -> Sem.Case Sem.Action Sem.Contract -> Element Msg
genCaseView blink (Sem.Case a c) =
    [ topHeader "Case of action"
    , subScope <| genActionView blink a
    , midHeader "do contract"
    , subScope <| genContractView blink c
    ]
        |> scopeBlock blink Hi.caseColor


genPayeeView : Blink -> Sem.Payee -> Element Msg
genPayeeView blink payee =
    case payee of
        Sem.Account id ->
            genAccountIdView blink id

        Sem.Party name ->
            genStringView blink name


genAccountIdView : Blink -> Sem.AccountId -> Element Msg
genAccountIdView blink (Sem.AccountId num owner) =
    [ topHeader "Account"
    , subScope <| genNumberView blink num
    , midHeader "with owner"
    , subScope <| genStringView blink owner
    ]
        |> scopeBlock blink Hi.accountId


genStringView : Blink -> String -> Element Msg
genStringView blink str =
    singletonHeader blink Hi.string <| "\"" ++ str ++ "\""


genNumberView : Blink -> Int -> Element Msg
genNumberView blink num =
    singletonHeader blink Hi.numColor <| String.fromInt num


genValueView : Blink -> Sem.Value Sem.Observation -> Element Msg
genValueView blink val =
    case val of
        Sem.AvailableMoney ->
            singletonHeader blink Hi.value <| "Available Money"

        Sem.Constant num ->
            singletonHeader blink Hi.value <| String.fromInt num

        Sem.NegValue v ->
            [ topHeader "negative"
            , subScope <| genValueView blink v
            ]
                |> scopeBlock blink Hi.value

        Sem.AddValue v1 v2 ->
            [ subScope <| genValueView blink v1
            , midHeader "plus"
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink Hi.value

        Sem.SubValue v1 v2 ->
            [ subScope <| genValueView blink v1
            , midHeader "minus"
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink Hi.value

        Sem.MulValue v1 v2 ->
            [ subScope <| genValueView blink v1
            , midHeader "times"
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink Hi.value

        Sem.Scale rat v ->
            [ topHeader "Scale by"
            , subScope <| genRationalView blink rat
            , midHeader "the value"
            , subScope <| genValueView blink v
            ]
                |> scopeBlock blink Hi.value

        Sem.ChoiceValue id v ->
            [ topHeader "A choice with"
            , subScope <| genChoiceIdView blink id
            , midHeader "defaulting to"
            , subScope <| genValueView blink v
            , endHeader "if no value is given"
            ]
                |> scopeBlock blink Hi.value

        Sem.SlotIntervalStart ->
            singletonHeader blink Hi.value "Start of slot interval"

        Sem.SlotIntervalEnd ->
            singletonHeader blink Hi.value "End of slot interval"

        Sem.UseValue id ->
            [ topHeader "Use value with identity"
            , subScope <| genNumberView blink id
            ]
                |> scopeBlock blink Hi.value

        Sem.Cond o a b ->
            [ topHeader "If it's observed that"
            , subScope <| genObservationView blink o
            , midHeader "then use the value"
            , subScope <| genValueView blink a
            , midHeader "otherwise use"
            , subScope <| genValueView blink b
            ]
                |> scopeBlock blink Hi.value


genObservationView : Blink -> Sem.Observation -> Element Msg
genObservationView blink obs =
    case obs of
        Sem.AndObs o1 o2 ->
            [ subScope <| genObservationView blink o1
            , midHeader "and"
            , subScope <| genObservationView blink o2
            ]
                |> scopeBlock blink Hi.andOr

        Sem.OrObs o1 o2 ->
            [ subScope <| genObservationView blink o1
            , midHeader "or"
            , subScope <| genObservationView blink o2
            ]
                |> scopeBlock blink Hi.andOr

        Sem.NotObs o ->
            [ topHeader "not"
            , subScope <| genObservationView blink o
            ]
                |> scopeBlock blink Hi.notColor

        Sem.ChooseSomething id ->
            [ topHeader "The choice"
            , subScope <| genChoiceIdView blink id
            ]
                |> scopeBlock blink Hi.chooseSomething

        Sem.ValueGE v1 v2 ->
            [ subScope <| genValueView blink v1
            , midHeader "is greater than or equal to"
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink Hi.valueGE

        Sem.ValueGT v1 v2 ->
            [ subScope <| genValueView blink v1
            , midHeader "is greater than"
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink Hi.valueGT

        Sem.ValueLT v1 v2 ->
            [ subScope <| genValueView blink v1
            , midHeader "is less than"
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink Hi.valueLT

        Sem.ValueLE v1 v2 ->
            [ subScope <| genValueView blink v1
            , midHeader "is less than or equal to"
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink Hi.valueLE

        Sem.ValueEQ v1 v2 ->
            [ subScope <| genValueView blink v1
            , midHeader "is equal to"
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink Hi.valueEQ

        Sem.TrueObs ->
            singletonHeader blink Hi.trueObs "True"

        Sem.FalseObs ->
            singletonHeader blink Hi.falseObs "False"


genActionView : Blink -> Sem.Action -> Element Msg
genActionView blink action =
    case action of
        Sem.Deposit id str val ->
            [ topHeader "Deposit to"
            , subScope <| genAccountIdView blink id
            , midHeader "from wallet of"
            , subScope <| genStringView blink str
            , midHeader "the amount of"
            , subScope <| genValueView blink val
            ]
                |> scopeBlock blink Hi.deposit

        Sem.Choice (Sem.ChoiceId name owner) bounds ->
            [ topHeader "A choice called"
            , subScope <| genStringView blink name
            , midHeader "was made by"
            , subScope <| genStringView blink owner
            , midHeader "with bounds"
            , column
                [ paddingEach { edges | top = gap, left = gap, bottom = gap }
                ]
                (bounds |> List.map (genBoundsView blink))
            , subScope <| singletonHeader blink Hi.bounds "..."
            ]
                |> scopeBlock blink Hi.deposit

        Sem.Notify obs ->
            [ topHeader "Notify when"
            , subScope <| genObservationView blink obs
            ]
                |> scopeBlock blink Hi.notify


genBoundsView : Blink -> Sem.Bound -> Element Msg
genBoundsView blink (Sem.Bound a b) =
    [ topHeader "Between"
    , subScope <| genNumberView blink a
    , midHeader "and"
    , subScope <| genNumberView blink b
    ]
        |> scopeBlock blink Hi.bounds


genChoiceIdView : Blink -> Sem.ChoiceId -> Element Msg
genChoiceIdView blink (Sem.ChoiceId choice owner) =
    [ topHeader "A value identified by"
    , subScope <| genStringView blink choice
    , midHeader "with owner"
    , subScope <| genStringView blink owner
    ]
        |> scopeBlock blink Hi.choiceId


genRationalView : Blink -> Sem.Rational -> Element Msg
genRationalView blink (Sem.Rational n d) =
    [ subScope <| genNumberView blink n
    , midHeader "over"
    , subScope <| genNumberView blink d
    ]
        |> scopeBlock blink Hi.rational



-- Managing View Complexity --


scopeBlock : Blink -> Color -> List (Element Msg) -> Element Msg
scopeBlock blink color elems =
    column
        [ Border.widthEach { edges | left = borderWidth }

        --, Border.dotted
        , Border.roundEach { corners | topLeft = margin, bottomLeft = margin }
        , Font.color <| blinkColor blink color
        ]
        elems


topHeader : String -> Element Msg
topHeader label =
    el
        [ Border.widthEach
            { edges
                | top = borderWidth
                , right = borderWidth
                , bottom = borderWidth
            }

        --, Border.dotted
        , Border.roundEach
            { corners
                | topLeft = margin
                , topRight = margin
                , bottomRight = margin
            }
        , paddingXY (gap * 2) gap
        ]
    <|
        text label


midHeader : String -> Element Msg
midHeader label =
    el
        [ Border.widthEach
            { edges
                | top = borderWidth
                , right = borderWidth
                , bottom = borderWidth
            }

        --, Border.dotted
        , Border.roundEach
            { corners
                | topRight = margin
                , bottomRight = margin
            }
        , paddingXY (gap * 2) gap
        ]
    <|
        text label


endHeader : String -> Element Msg
endHeader label =
    el
        [ Border.widthEach
            { edges
                | top = borderWidth
                , right = borderWidth
                , bottom = borderWidth
            }

        --, Border.dotted
        , Border.roundEach
            { corners
                | bottomLeft = margin
                , topRight = margin
                , bottomRight = margin
            }
        , paddingXY (margin * 2) margin
        ]
    <|
        text label


singletonHeader : Blink -> Color -> String -> Element Msg
singletonHeader blink color label =
    el
        [ Font.color <| blinkColor blink color
        , Border.width borderWidth

        --, Border.dotted
        , Border.rounded margin
        , paddingXY (gap * 2) gap
        ]
    <|
        text label


subScope : Element Msg -> Element Msg
subScope elem =
    el
        [ paddingEach { edges | top = margin, left = margin, bottom = margin }
        , width fill
        ]
        elem



-- Keyboard Extras --


keyboardButton : Blink -> Color -> String -> Element Msg
keyboardButton blink color label =
    el
        [ Font.color <| blinkColor blink color
        , Font.center
        , Border.width borderWidth
        , Border.rounded margin
        , width fill
        , paddingXY (margin * 2) margin

        --, height fill
        ]
    <|
        el [ centerY, centerX ] <|
            text label
