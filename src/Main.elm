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
    , menuVisible : Bool
    , viewport : Dom.Viewport
    }


type Msg
    = NewUrl Url.Url
    | Link Browser.UrlRequest
    | TimeDelta Float
    | SwitchContract SampleContract
    | ToggleKeyboard
    | ToggleMenu
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
    ( Model key url 0.0 Regular Sem.escrow False False dummyViewport
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

        ToggleMenu ->
            ( { model | menuVisible = not model.menuVisible }
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
            [ width <| px <| round model.viewport.viewport.width
            , height <| px <| round model.viewport.viewport.height
            , Bg.color Hi.bgBlue
            , Font.color Hi.accentPink
            , Font.size 30
            , Font.family
                [ Font.typeface "Helvetica"
                , Font.sansSerif
                ]
            , inFront <|
                if model.keyboardState then
                    Kb.kb model.viewport.viewport.height
                    --model.sampleContract

                else
                    none
            ]
          <|
            column
                [ width <| px <| round model.viewport.viewport.width
                , height <| px <| round model.viewport.viewport.height
                , clip
                , scrollbarY
                ]
                [ row
                    [ Font.size 60
                    , Font.letterSpacing 1.5
                    , Font.color Hi.white
                    , Border.widthEach { edges | bottom = borderWidth }
                    , Border.dotted
                    , Border.color Hi.accentPink
                    , width fill
                    , paddingXY (margin * 2) margin
                    ]
                    [ el [ Font.bold ] <|
                        text "Marlowe "
                    , el [] <|
                        text "Mobile"
                    , el
                        [ alignRight
                        , padding margin
                        , below <|
                            if model.menuVisible then
                                column
                                    [ height shrink
                                    , width shrink

                                    --, Bg.color <| rgb 1 0 0
                                    , Font.size 20
                                    , Font.color Hi.accentPink
                                    , Border.width borderWidth
                                    , Bg.color <| Hi.keyBg 0.75 Hi.bgBlue
                                    , Border.dotted
                                    , alignRight
                                    , padding <| margin * 2
                                    , spacing <| margin * 4
                                    ]
                                    [ sampleButton CouponBond "CouponBondGuaranteed"
                                    , sampleButton Escrow "Escrow"
                                    , sampleButton Swap "Swap"
                                    , sampleButton ZeroCoupon "ZeroCouponBond"
                                    , sampleButton NilContract "Nil"
                                    ]

                            else
                                none
                        , ElEvent.onClick ToggleMenu
                        ]
                      <|
                        text "≣"
                    ]
                , el
                    [ width fill
                    , height fill
                    , padding <| 2 * margin
                    , Bg.color Hi.black
                    , ElEvent.onClick ToggleKeyboard
                    ]
                  <|
                    ([ topHeaderPlain "Contract such that"

                     -- TODO: model.blink along with the unique id of the subtree where blink should
                     -- start if something is selected should be passed to genContractView.
                     , subScope <|
                        genContractView Regular <|
                            Sem.annotateContract model.sampleContract
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


sampleButton : SampleContract -> String -> Element Msg
sampleButton c s =
    Input.button
        [ width fill
        , Border.width borderWidth
        , Border.rounded margin
        , padding margin
        ]
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


genContractView : Blink -> Sem.AnnoContract -> Element Msg
genContractView blink contract =
    case contract of
        Sem.AnnoRefund anno ->
            singletonHeader blink Hi.refund "Refund remaining" anno

        Sem.AnnoPay p1 p2 v a anno ->
            [ topHeader "Pay from" anno
            , subScope <| genAccountIdView blink p1
            , midHeader "to" anno
            , subScope <| genPayeeView blink p2
            , midHeader "the amount of" anno
            , subScope <| genValueView blink v
            , midHeader "and continue with" anno
            , subScope <| genContractView blink a
            ]
                |> scopeBlock blink Hi.pay

        Sem.AnnoIf o a b anno ->
            [ topHeader "If" anno
            , subScope <| genObservationView blink o
            , midHeader "then" anno
            , subScope <| genContractView blink a
            , midHeader "else" anno
            , subScope <| genContractView blink b
            ]
                |> scopeBlock blink Hi.ifColor

        Sem.AnnoWhen cases t y anno ->
            [ topHeader "When" anno
            , column
                [ paddingEach { edges | top = margin, left = margin, bottom = margin }
                ]
                (cases |> List.map (genCaseView blink))
            , subScope <| singletonHeader blink Hi.caseColor "..." anno
            , midHeader "after slot" anno
            , subScope <| genNumberView blink t
            , midHeader "continue as" anno
            , subScope <| genContractView blink y
            ]
                |> scopeBlock blink Hi.contractColor

        Sem.AnnoLet id val a anno ->
            [ topHeader "Let the number" anno
            , subScope <| genNumberView blink id
            , midHeader "identify the value" anno
            , subScope <| genValueView blink val
            , midHeader "in" anno
            , subScope <| genContractView blink a
            ]
                |> scopeBlock blink Hi.letColor


genCaseView : Blink -> Sem.AnnoCase Sem.AnnoAction Sem.AnnoContract -> Element Msg
genCaseView blink (Sem.AnnoCase a c anno) =
    [ topHeader "Case of action" anno
    , subScope <| genActionView blink a
    , midHeader "do contract" anno
    , subScope <| genContractView blink c
    ]
        |> scopeBlock blink Hi.caseColor


genPayeeView : Blink -> Sem.AnnoPayee -> Element Msg
genPayeeView blink payee =
    case payee of
        Sem.AnnoAccount id anno ->
            genAccountIdView blink id

        Sem.AnnoParty name anno ->
            genPartyView blink name


genAccountIdView : Blink -> Sem.AnnoAccountId -> Element Msg
genAccountIdView blink (Sem.AnnoAccountId num owner anno) =
    [ topHeader "Account" anno
    , subScope <| genNumberView blink num
    , midHeader "with owner" anno
    , subScope <| genPartyView blink owner
    ]
        |> scopeBlock blink Hi.accountId


genPartyView : Blink -> Sem.AnnoString -> Element Msg
genPartyView blink name =
    genStringView blink name


genStringView : Blink -> Sem.AnnoString -> Element Msg
genStringView blink (Sem.AnnoString str anno) =
    singletonHeader blink Hi.string ("\"" ++ str ++ "\"") anno


genNumberView : Blink -> Sem.AnnoNum -> Element Msg
genNumberView blink (Sem.AnnoNum num anno) =
    singletonHeader blink Hi.numColor (String.fromInt num) anno


genValueView : Blink -> Sem.AnnoValue Sem.AnnoObservation -> Element Msg
genValueView blink val =
    case val of
        Sem.AnnoAvailableMoney anno ->
            singletonHeader blink Hi.value "Available Money" anno

        Sem.AnnoConstant num anno ->
            genNumberView blink num

        Sem.AnnoNegValue v anno ->
            [ topHeader "negative" anno
            , subScope <| genValueView blink v
            ]
                |> scopeBlock blink Hi.value

        Sem.AnnoAddValue v1 v2 anno ->
            [ subScope <| genValueView blink v1
            , midHeader "plus" anno
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink Hi.value

        Sem.AnnoSubValue v1 v2 anno ->
            [ subScope <| genValueView blink v1
            , midHeader "minus" anno
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink Hi.value

        Sem.AnnoMulValue v1 v2 anno ->
            [ subScope <| genValueView blink v1
            , midHeader "times" anno
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink Hi.value

        Sem.AnnoScale rat v anno ->
            [ topHeader "Scale by" anno
            , subScope <| genRationalView blink rat
            , midHeader "the value" anno
            , subScope <| genValueView blink v
            ]
                |> scopeBlock blink Hi.value

        Sem.AnnoChoiceValue id v anno ->
            [ topHeader "A choice with" anno
            , subScope <| genChoiceIdView blink id
            , midHeader "defaulting to" anno
            , subScope <| genValueView blink v
            , endHeader "if no value is given" anno
            ]
                |> scopeBlock blink Hi.value

        Sem.AnnoSlotIntervalStart anno ->
            singletonHeader blink Hi.value "Start of slot interval" anno

        Sem.AnnoSlotIntervalEnd anno ->
            singletonHeader blink Hi.value "End of slot interval" anno

        Sem.AnnoUseValue id anno ->
            [ topHeader "Use value with identity" anno
            , subScope <| genNumberView blink id
            ]
                |> scopeBlock blink Hi.value

        Sem.AnnoCond o a b anno ->
            [ topHeader "If it's observed that" anno
            , subScope <| genObservationView blink o
            , midHeader "then use the value" anno
            , subScope <| genValueView blink a
            , midHeader "otherwise use" anno
            , subScope <| genValueView blink b
            ]
                |> scopeBlock blink Hi.value


genObservationView : Blink -> Sem.AnnoObservation -> Element Msg
genObservationView blink obs =
    case obs of
        Sem.AnnoAndObs o1 o2 anno ->
            [ subScope <| genObservationView blink o1
            , midHeader "and" anno
            , subScope <| genObservationView blink o2
            ]
                |> scopeBlock blink Hi.andOr

        Sem.AnnoOrObs o1 o2 anno ->
            [ subScope <| genObservationView blink o1
            , midHeader "or" anno
            , subScope <| genObservationView blink o2
            ]
                |> scopeBlock blink Hi.andOr

        Sem.AnnoNotObs o anno ->
            [ topHeader "not" anno
            , subScope <| genObservationView blink o
            ]
                |> scopeBlock blink Hi.notColor

        Sem.AnnoChooseSomething id anno ->
            [ topHeader "The choice" anno
            , subScope <| genChoiceIdView blink id
            ]
                |> scopeBlock blink Hi.chooseSomething

        Sem.AnnoValueGE v1 v2 anno ->
            [ subScope <| genValueView blink v1
            , midHeader "is greater than or equal to" anno
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink Hi.valueGE

        Sem.AnnoValueGT v1 v2 anno ->
            [ subScope <| genValueView blink v1
            , midHeader "is greater than" anno
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink Hi.valueGT

        Sem.AnnoValueLT v1 v2 anno ->
            [ subScope <| genValueView blink v1
            , midHeader "is less than" anno
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink Hi.valueLT

        Sem.AnnoValueLE v1 v2 anno ->
            [ subScope <| genValueView blink v1
            , midHeader "is less than or equal to" anno
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink Hi.valueLE

        Sem.AnnoValueEQ v1 v2 anno ->
            [ subScope <| genValueView blink v1
            , midHeader "is equal to" anno
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink Hi.valueEQ

        Sem.AnnoTrueObs anno ->
            singletonHeader blink Hi.trueObs "True" anno

        Sem.AnnoFalseObs anno ->
            singletonHeader blink Hi.falseObs "False" anno


genActionView : Blink -> Sem.AnnoAction -> Element Msg
genActionView blink action =
    case action of
        Sem.AnnoDeposit id str val anno ->
            [ topHeader "Deposit to" anno
            , subScope <| genAccountIdView blink id
            , midHeader "from wallet of" anno
            , subScope <| genStringView blink str
            , midHeader "the amount of" anno
            , subScope <| genValueView blink val
            ]
                |> scopeBlock blink Hi.deposit

        Sem.AnnoChoice choice bounds anno ->
            [ topHeader "A choice" anno
            , subScope <| genChoiceIdView blink choice

            --, midHeader "was made by" anno
            --, subScope <| genStringView blink owner
            , midHeader "with bounds" anno
            , column
                [ paddingEach { edges | top = gap, left = gap, bottom = gap }
                ]
                (bounds |> List.map (genBoundsView blink))
            , subScope <| singletonHeader blink Hi.bounds "..." anno
            ]
                |> scopeBlock blink Hi.deposit

        Sem.AnnoNotify obs anno ->
            [ topHeader "Notify when" anno
            , subScope <| genObservationView blink obs
            ]
                |> scopeBlock blink Hi.notify


genBoundsView : Blink -> Sem.AnnoBound -> Element Msg
genBoundsView blink (Sem.AnnoBound a b anno) =
    [ topHeader "Between" anno
    , subScope <| genNumberView blink a
    , midHeader "and" anno
    , subScope <| genNumberView blink b
    ]
        |> scopeBlock blink Hi.bounds


genChoiceIdView : Blink -> Sem.AnnoChoiceId -> Element Msg
genChoiceIdView blink (Sem.AnnoChoiceId choice owner anno) =
    [ topHeader "A value identified by" anno
    , subScope <| genStringView blink choice
    , midHeader "with owner" anno
    , subScope <| genStringView blink owner
    ]
        |> scopeBlock blink Hi.choiceId


genRationalView : Blink -> Sem.AnnoRational -> Element Msg
genRationalView blink (Sem.AnnoRational n d anno) =
    [ subScope <| genNumberView blink n
    , midHeader "over" anno
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


topHeader : String -> Sem.Anno -> Element Msg
topHeader label anno =
    topHeaderPlain label


topHeaderPlain : String -> Element Msg
topHeaderPlain label =
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


midHeader : String -> Sem.Anno -> Element Msg
midHeader label anno =
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


endHeader : String -> Sem.Anno -> Element Msg
endHeader label anno =
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


singletonHeader : Blink -> Color -> String -> Sem.Anno -> Element Msg
singletonHeader blink color label anno =
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
