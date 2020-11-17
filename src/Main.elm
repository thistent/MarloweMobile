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
import Marlowe.Semantics as Sem exposing (Msg(..), SampleContract(..))
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
    , display : Dom.Viewport
    , selected : Maybe Sem.Anno
    }


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
    ( Model key url 0.0 Regular Sem.escrow False False dummyViewport Nothing
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
            ( { model | display = vp }, Cmd.none )

        Select maybeAnno ->
            ( { model
                | selected = maybeAnno
                , keyboardState = True --not model.keyboardState
              }
            , Cmd.none
            )



-- View Function --


view : Model -> Browser.Document Msg
view model =
    { title = "Marlowe Mobile"
    , body =
        [ Element.layout
            [ width fill
            , height fill
            , Font.size <| round <| model.display.viewport.width / 20
            , Font.color Hi.accentPink
            , Bg.color Hi.black
            , Font.family
                [ Font.typeface "Helvetica"
                , Font.sansSerif
                ]
            , inFront <| topRow model
            , inFront <|
                if model.keyboardState then
                    Kb.kb model.display.viewport

                else
                    none
            ]
          <|
            el
                [ width fill
                , height fill --<| px <| round model.viewport.viewport.height
                , Bg.color Hi.black
                , paddingEach
                    { edges
                        | left = 2 * margin
                        , right = 2 * margin
                        , top = round <| model.display.viewport.width / 5.5
                        , bottom = round <| model.display.viewport.height / 2
                    }

                --, ElEvent.onClick ToggleKeyboard
                , clipY
                , scrollbarY
                ]
                ([ topHeader Regular Hi.contractBase "Contract such that" Nothing
                 , subScope <|
                    genContractView Regular <|
                        Sem.annotateContract model.sampleContract
                 ]
                    |> scopeBlock Regular Hi.contractBase
                )
        ]
    }


topRow : Model -> Element Msg
topRow model =
    row
        [ Font.size <| round <| model.display.viewport.width / 12
        , Font.letterSpacing 1.5
        , Font.color Hi.white
        , Bg.color <| Hi.addAlpha 0.9 Hi.bgBlue
        , Border.widthEach { edges | bottom = borderWidth }
        , Border.dotted
        , Border.color Hi.accentPink
        , width fill
        , height shrink
        , paddingXY (margin * 2) margin
        ]
        [ el [ Font.bold ] <|
            text "Marlowe "
        , el [] <|
            text "Mobile"
        , Input.button
            [ padding margin
            , alignRight
            , below <|
                if model.menuVisible then
                    column
                        [ height shrink
                        , width shrink
                        , Font.size <| round <| model.display.viewport.width / 35
                        , Font.color Hi.accentPink
                        , Border.width borderWidth
                        , Bg.color <| Hi.addAlpha 0.9 Hi.bgBlue
                        , Border.dotted
                        , alignRight
                        , padding <| margin * 2
                        , spacing <| margin * 4
                        ]
                        [ el [ width fill, padding margin ] <|
                            text <|
                                String.fromFloat model.display.viewport.width
                                    ++ " x "
                                    ++ String.fromFloat model.display.viewport.height
                                    ++ " screen"
                        , sampleButton CouponBond "CouponBondGuaranteed"
                        , sampleButton Escrow "Escrow"
                        , sampleButton Swap "Swap"
                        , sampleButton ZeroCoupon "ZeroCouponBond"
                        , sampleButton NilContract "Nil"
                        ]

                else
                    none
            ]
            { onPress = Just ToggleMenu
            , label = text "≣"
            }
        ]


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
            singletonHeader blink Hi.refund "Refund remaining" <| Just anno

        Sem.AnnoPay p1 p2 v a anno ->
            [ topHeader blink Hi.pay "Pay from" <| Just anno
            , subScope <| genAccountIdView blink p1
            , midHeader blink Hi.pay "to" <| Just anno
            , subScope <| genPayeeView blink p2
            , midHeader blink Hi.pay "the amount of" <| Just anno
            , subScope <| genValueView blink v
            , midHeader blink Hi.pay "and continue with" <| Just anno
            , subScope <| genContractView blink a
            ]
                |> scopeBlock blink Hi.pay

        Sem.AnnoIf o a b anno ->
            [ topHeader blink Hi.ifColor "If" <| Just anno
            , subScope <| genObservationView blink o
            , midHeader blink Hi.ifColor "then" <| Just anno
            , subScope <| genContractView blink a
            , midHeader blink Hi.ifColor "else" <| Just anno
            , subScope <| genContractView blink b
            ]
                |> scopeBlock blink Hi.ifColor

        Sem.AnnoWhen cases t y anno ->
            [ topHeader blink Hi.contractColor "When" <| Just anno
            , column
                [ paddingEach { edges | top = 15, left = 15, bottom = 15 }
                ]
                (cases |> List.map (genCaseView blink))
            , subScope <| singletonHeader blink Hi.caseColor "..." <| Just anno
            , midHeader blink Hi.contractColor "after slot" <| Just anno
            , subScope <| genNumberView blink t
            , midHeader blink Hi.contractColor "continue as" <| Just anno
            , subScope <| genContractView blink y
            ]
                |> scopeBlock blink Hi.contractColor

        Sem.AnnoLet id val a anno ->
            [ topHeader blink Hi.letColor "Let the number" <| Just anno
            , subScope <| genNumberView blink id
            , midHeader blink Hi.letColor "identify the value" <| Just anno
            , subScope <| genValueView blink val
            , midHeader blink Hi.letColor "in" <| Just anno
            , subScope <| genContractView blink a
            ]
                |> scopeBlock blink Hi.letColor


genCaseView : Blink -> Sem.AnnoCase Sem.AnnoAction Sem.AnnoContract -> Element Msg
genCaseView blink (Sem.AnnoCase a c anno) =
    [ topHeader blink Hi.caseColor "Case of action" <| Just anno
    , subScope <| genActionView blink a
    , midHeader blink Hi.caseColor "do contract" <| Just anno
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
    [ topHeader blink Hi.accountId "Account" <| Just anno
    , subScope <| genNumberView blink num
    , midHeader blink Hi.accountId "with owner" <| Just anno
    , subScope <| genPartyView blink owner
    ]
        |> scopeBlock blink Hi.accountId


genPartyView : Blink -> Sem.AnnoString -> Element Msg
genPartyView blink name =
    genStringView blink name


genStringView : Blink -> Sem.AnnoString -> Element Msg
genStringView blink (Sem.AnnoString str anno) =
    singletonHeader blink Hi.string ("\"" ++ str ++ "\"") <| Just anno


genNumberView : Blink -> Sem.AnnoNum -> Element Msg
genNumberView blink (Sem.AnnoNum num anno) =
    singletonHeader blink Hi.numColor (String.fromInt num) <| Just anno


genValueView : Blink -> Sem.AnnoValue Sem.AnnoObservation -> Element Msg
genValueView blink val =
    case val of
        Sem.AnnoAvailableMoney anno ->
            singletonHeader blink Hi.value "Available Money" <| Just anno

        Sem.AnnoConstant num anno ->
            genNumberView blink num

        Sem.AnnoNegValue v anno ->
            [ topHeader blink Hi.value "negative" <| Just anno
            , subScope <| genValueView blink v
            ]
                |> scopeBlock blink Hi.value

        Sem.AnnoAddValue v1 v2 anno ->
            [ subScope <| genValueView blink v1
            , midHeader blink Hi.value "plus" <| Just anno
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink Hi.value

        Sem.AnnoSubValue v1 v2 anno ->
            [ subScope <| genValueView blink v1
            , midHeader blink Hi.value "minus" <| Just anno
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink Hi.value

        Sem.AnnoMulValue v1 v2 anno ->
            [ subScope <| genValueView blink v1
            , midHeader blink Hi.value "times" <| Just anno
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink Hi.value

        Sem.AnnoScale rat v anno ->
            [ topHeader blink Hi.value "Scale by" <| Just anno
            , subScope <| genRationalView blink rat
            , midHeader blink Hi.value "the value" <| Just anno
            , subScope <| genValueView blink v
            ]
                |> scopeBlock blink Hi.value

        Sem.AnnoChoiceValue id v anno ->
            [ topHeader blink Hi.value "A choice of" <| Just anno
            , subScope <| genChoiceIdView blink id
            , midHeader blink Hi.value "with a default value of" <| Just anno
            , subScope <| genValueView blink v

            --, endHeader "if no value is given" <| Just anno
            ]
                |> scopeBlock blink Hi.value

        Sem.AnnoSlotIntervalStart anno ->
            singletonHeader blink Hi.value "Start of slot interval" <| Just anno

        Sem.AnnoSlotIntervalEnd anno ->
            singletonHeader blink Hi.value "End of slot interval" <| Just anno

        Sem.AnnoUseValue id anno ->
            [ topHeader blink Hi.value "Use value with identity" <| Just anno
            , subScope <| genNumberView blink id
            ]
                |> scopeBlock blink Hi.value

        Sem.AnnoCond o a b anno ->
            [ topHeader blink Hi.value "If it's observed that" <| Just anno
            , subScope <| genObservationView blink o
            , midHeader blink Hi.value "then use the value" <| Just anno
            , subScope <| genValueView blink a
            , midHeader blink Hi.value "otherwise use" <| Just anno
            , subScope <| genValueView blink b
            ]
                |> scopeBlock blink Hi.value


genObservationView : Blink -> Sem.AnnoObservation -> Element Msg
genObservationView blink obs =
    case obs of
        Sem.AnnoAndObs o1 o2 anno ->
            [ subScope <| genObservationView blink o1
            , midHeader blink Hi.andOr "and" <| Just anno
            , subScope <| genObservationView blink o2
            ]
                |> scopeBlock blink Hi.andOr

        Sem.AnnoOrObs o1 o2 anno ->
            [ subScope <| genObservationView blink o1
            , midHeader blink Hi.andOr "or" <| Just anno
            , subScope <| genObservationView blink o2
            ]
                |> scopeBlock blink Hi.andOr

        Sem.AnnoNotObs o anno ->
            [ topHeader blink Hi.notColor "not" <| Just anno
            , subScope <| genObservationView blink o
            ]
                |> scopeBlock blink Hi.notColor

        Sem.AnnoChooseSomething id anno ->
            [ topHeader blink Hi.chooseSomething "This choice is made" <| Just anno
            , subScope <| genChoiceIdView blink id
            ]
                |> scopeBlock blink Hi.chooseSomething

        Sem.AnnoValueGE v1 v2 anno ->
            [ subScope <| genValueView blink v1
            , midHeader blink Hi.valueGE "is greater than or equal to" <| Just anno
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink Hi.valueGE

        Sem.AnnoValueGT v1 v2 anno ->
            [ subScope <| genValueView blink v1
            , midHeader blink Hi.valueGT "is greater than" <| Just anno
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink Hi.valueGT

        Sem.AnnoValueLT v1 v2 anno ->
            [ subScope <| genValueView blink v1
            , midHeader blink Hi.valueLT "is less than" <| Just anno
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink Hi.valueLT

        Sem.AnnoValueLE v1 v2 anno ->
            [ subScope <| genValueView blink v1
            , midHeader blink Hi.valueLE "is less than or equal to" <| Just anno
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink Hi.valueLE

        Sem.AnnoValueEQ v1 v2 anno ->
            [ subScope <| genValueView blink v1
            , midHeader blink Hi.valueEQ "is equal to" <| Just anno
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink Hi.valueEQ

        Sem.AnnoTrueObs anno ->
            singletonHeader blink Hi.trueObs "True" <| Just anno

        Sem.AnnoFalseObs anno ->
            singletonHeader blink Hi.falseObs "False" <| Just anno


genActionView : Blink -> Sem.AnnoAction -> Element Msg
genActionView blink action =
    case action of
        Sem.AnnoDeposit id str val anno ->
            [ topHeader blink Hi.deposit "Deposit to" <| Just anno
            , subScope <| genAccountIdView blink id
            , midHeader blink Hi.deposit "from wallet of" <| Just anno
            , subScope <| genStringView blink str
            , midHeader blink Hi.deposit "the amount of" <| Just anno
            , subScope <| genValueView blink val
            ]
                |> scopeBlock blink Hi.deposit

        Sem.AnnoChoice choice bounds anno ->
            [ topHeader blink Hi.choice "A choice made" <| Just anno
            , subScope <| genChoiceIdView blink choice
            , midHeader blink Hi.choice "with bounds" <| Just anno
            , column
                [ paddingEach { edges | top = 15, left = 15, bottom = 15 }
                ]
                (bounds |> List.map (genBoundsView blink))
            , subScope <| singletonHeader blink Hi.bounds "..." <| Just anno
            ]
                |> scopeBlock blink Hi.choice

        Sem.AnnoNotify obs anno ->
            [ topHeader blink Hi.notify "Notify when" <| Just anno
            , subScope <| genObservationView blink obs
            ]
                |> scopeBlock blink Hi.notify


genBoundsView : Blink -> Sem.AnnoBound -> Element Msg
genBoundsView blink (Sem.AnnoBound a b anno) =
    [ topHeader blink Hi.bounds "Between" <| Just anno
    , subScope <| genNumberView blink a
    , midHeader blink Hi.bounds "and" <| Just anno
    , subScope <| genNumberView blink b
    ]
        |> scopeBlock blink Hi.bounds


genChoiceIdView : Blink -> Sem.AnnoChoiceId -> Element Msg
genChoiceIdView blink (Sem.AnnoChoiceId choice owner anno) =
    [ topHeader blink Hi.choiceId "The value identified by" <| Just anno
    , subScope <| genStringView blink choice
    , midHeader blink Hi.choiceId "and owned by" <| Just anno
    , subScope <| genStringView blink owner
    ]
        |> scopeBlock blink Hi.choiceId


genRationalView : Blink -> Sem.AnnoRational -> Element Msg
genRationalView blink (Sem.AnnoRational n d anno) =
    [ subScope <| genNumberView blink n
    , midHeader blink Hi.rational "over" <| Just anno
    , subScope <| genNumberView blink d
    ]
        |> scopeBlock blink Hi.rational



-- Managing View Complexity --


scopeBlock : Blink -> Color -> List (Element Msg) -> Element Msg
scopeBlock blink color elems =
    column
        [ Border.widthEach { edges | left = borderWidth }
        , Border.roundEach { corners | topLeft = margin, bottomLeft = margin }
        , Font.color <| blinkColor blink color
        ]
        elems


topHeader : Blink -> Color -> String -> Maybe Sem.Anno -> Element Msg
topHeader blink color label maybeAnno =
    let
        style =
            [ Border.widthEach
                { edges
                    | top = borderWidth
                    , right = borderWidth
                    , bottom = borderWidth
                }
            , Border.roundEach
                { corners
                    | topLeft = margin
                    , topRight = margin
                    , bottomRight = margin
                }
            , Font.color <| blinkColor blink color
            , Bg.color <| Hi.addAlpha 0.2 color

            --, Bg.color Hi.bgBlue
            , Font.justify
            , paddingXY (gap * 2) gap
            ]
    in
    maybeButton style label maybeAnno


midHeader : Blink -> Color -> String -> Maybe Sem.Anno -> Element Msg
midHeader blink color label maybeAnno =
    let
        style =
            [ Border.widthEach
                { edges
                    | top = borderWidth
                    , right = borderWidth
                    , bottom = borderWidth
                }
            , Border.roundEach
                { corners
                    | topRight = margin
                    , bottomRight = margin
                }

            --, Bg.color Hi.bgBlue
            , Font.color <| blinkColor blink color
            , Bg.color <| Hi.addAlpha 0.2 color
            , Font.justify
            , paddingXY (gap * 2) gap
            ]
    in
    maybeButton style label maybeAnno



{-
   endHeader : String -> Maybe Sem.Anno -> Element Msg
   endHeader label maybeAnno =
       let
           style =
               [ Border.widthEach
                   { edges
                       | top = borderWidth
                       , right = borderWidth
                       , bottom = borderWidth
                   }
               , Border.roundEach
                   { corners
                       | bottomLeft = margin
                       , topRight = margin
                       , bottomRight = margin
                   }
               , Bg.color Hi.bgBlue
            , Font.justify
               , paddingXY (margin * 2) margin
               ]
       in
       maybeButton style label maybeAnno

-}


maybeButton : List (Attribute Msg) -> String -> Maybe Sem.Anno -> Element Msg
maybeButton style label maybeAnno =
    case maybeAnno of
        Nothing ->
            el style <| paragraph [] [ text label ]

        Just _ ->
            Input.button style
                { onPress = Just <| Select maybeAnno
                , label = paragraph [] [ text label ]
                }


singletonHeader : Blink -> Color -> String -> Maybe Sem.Anno -> Element Msg
singletonHeader blink color label maybeAnno =
    let
        style =
            [ Font.color <| blinkColor blink color
            , Border.width borderWidth
            , Border.rounded margin
            , Bg.color <| Hi.addAlpha 0.2 color
            , Font.justify
            , paddingXY (gap * 2) gap
            ]
    in
    maybeButton style label maybeAnno


subScope : Element Msg -> Element Msg
subScope elem =
    el
        [ paddingEach { edges | top = 15, left = 15, bottom = 15 }
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
