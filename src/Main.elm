module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Browser.Navigation as Nav
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
    , blink : Hi.Blink
    , sampleContract : Sem.Contract
    , keyboardState : Bool
    , menuVisible : Bool
    , display : Dom.Viewport
    , selected : Maybe Sem.Anno
    }



-- Highlight Theme --


theme : Hi.Theme
theme =
    Hi.lightTheme



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
    ( Model key url 0.0 Hi.Empty Sem.escrow False False dummyViewport Nothing
    , Cmd.batch
        [ Task.perform UpdateViewport Dom.getViewport
        ]
    )



-- Subscriptions --


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ Events.onAnimationFrameDelta TimeDelta
        , Events.onResize ScreenSize
        ]



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
                            Hi.Empty

                        else if newDelta <= 400 then
                            Hi.HalfFull

                        else if newDelta <= 600 then
                            Hi.Full

                        else
                            Hi.HalfEmpty
                    , timeDelta = model.timeDelta + delta
                  }
                , Cmd.none
                )

            else
                ( { model | timeDelta = 0 }
                , Cmd.none
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
                    ( { model | sampleContract = Sem.Close }
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

        ScreenSize _ _ ->
            ( model
            , Cmd.batch
                [ Task.perform UpdateViewport Dom.getViewport
                ]
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
            , Font.color theme.accent
            , Bg.color theme.bg
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
                , Bg.color theme.bg
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
                ([ topHeader Hi.Empty theme.contract "Contract such that" Nothing
                 , subScope <|
                    genContractView Hi.Empty {- model.blink -} <|
                        Sem.annotateContract model.sampleContract
                 ]
                    |> scopeBlock Hi.Empty theme.contract
                )
        ]
    }


topRow : Model -> Element Msg
topRow model =
    row
        [ Font.size <| round <| model.display.viewport.width / 12
        , Font.letterSpacing 1.5
        , Font.color theme.fg
        , Bg.color <| Hi.addAlpha 0.9 theme.barBg
        , Border.widthEach { edges | bottom = borderWidth }
        , Border.dotted
        , Border.color theme.accent
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
                        , Font.color theme.accent
                        , Border.width borderWidth
                        , Bg.color <| Hi.addAlpha 0.9 theme.barBg
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
        , paddingXY (margin * 2) margin
        ]
        { onPress = Just (SwitchContract c)
        , label = text s
        }


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


genContractView : Hi.Blink -> Sem.AnnoContract -> Element Msg
genContractView blink contract =
    let
        color =
            theme.contract
    in
    case contract of
        Sem.AnnoClose anno ->
            singletonHeader blink color "Close Contract" <| Just anno

        Sem.AnnoPay p1 p2 tok v a anno ->
            [ topHeader blink color "Pay from" <| Just anno
            , subScope <| genPartyView blink p1
            , midHeader blink color "to" <| Just anno
            , subScope <| genPayeeView blink p2
            , midHeader blink color "the amount of" <| Just anno
            , subScope <| genValueView blink v
            , midHeader blink color "of currency" <| Just anno
            , subScope <| genTokenView blink tok
            , midHeader blink color "and continue with" <| Just anno
            , subScope <| genContractView blink a
            ]
                |> scopeBlock blink color

        Sem.AnnoIf o a b anno ->
            [ topHeader blink color "If" <| Just anno
            , subScope <| genObservationView blink o
            , midHeader blink color "then" <| Just anno
            , subScope <| genContractView blink a
            , midHeader blink color "else" <| Just anno
            , subScope <| genContractView blink b
            ]
                |> scopeBlock blink color

        Sem.AnnoWhen cases t y anno ->
            [ topHeader blink color "When" <| Just anno
            , column
                [ paddingEach { edges | top = 15, left = 15, bottom = 15 }
                , spacing 15
                ]
                (cases
                    |> List.map (genCaseView blink)
                    |> (\x ->
                            (++) x
                                [ singletonHeader blink theme.caseColor "..." <| Just anno ]
                       )
                )
            , midHeader blink color "after slot" <| Just anno
            , subScope <| genNumberView blink color t
            , midHeader blink color "continue as" <| Just anno
            , subScope <| genContractView blink y
            ]
                |> scopeBlock blink color

        Sem.AnnoLet id val a anno ->
            [ topHeader blink color "Let the number" <| Just anno
            , subScope <| genNumberView blink color id
            , midHeader blink color "identify the value" <| Just anno
            , subScope <| genValueView blink val
            , midHeader blink color "in" <| Just anno
            , subScope <| genContractView blink a
            ]
                |> scopeBlock blink color


genCaseView : Hi.Blink -> Sem.AnnoCase Sem.AnnoAction Sem.AnnoContract -> Element Msg
genCaseView blink (Sem.AnnoCase a c anno) =
    let
        color =
            theme.caseColor
    in
    [ topHeader blink color "Case" <| Just anno
    , subScope <| genActionView blink a
    , midHeader blink color "continue as" <| Just anno
    , subScope <| genContractView blink c
    ]
        |> scopeBlock blink color


genPayeeView : Hi.Blink -> Sem.AnnoPayee -> Element Msg
genPayeeView blink payee =
    case payee of
        Sem.AnnoAccount id anno ->
            genAccountIdView blink id

        Sem.AnnoParty name anno ->
            genPartyView blink name


genAccountIdView : Hi.Blink -> Sem.AnnoAccountId -> Element Msg
genAccountIdView blink (Sem.AnnoAccountId num owner anno) =
    let
        color =
            theme.payee
    in
    [ topHeader blink color "Account" <| Just anno
    , subScope <| genNumberView blink color num
    , midHeader blink color "with owner" <| Just anno
    , subScope <| genStringView blink color owner
    ]
        |> scopeBlock blink color


genPartyView : Hi.Blink -> Sem.AnnoParty -> Element Msg
genPartyView blink party =
    let
        color =
            theme.party
    in
    case party of
        Sem.AnnoPK str anno ->
            [ topHeader blink color "Public Key" <| Just anno
            , subScope <| genStringView blink color str
            ]
                |> scopeBlock blink color

        Sem.AnnoRole str anno ->
            [ topHeader blink color "Role" <| Just anno
            , subScope <| genStringView blink color str
            ]
                |> scopeBlock blink color


genStringView : Hi.Blink -> Color -> Sem.AnnoString -> Element Msg
genStringView blink color (Sem.AnnoString str anno) =
    let
        newColor =
            Hi.lighten color
    in
    singletonHeader blink
        newColor
        ("\"" ++ str ++ "\"")
    <|
        Just anno


genNumberView : Hi.Blink -> Color -> Sem.AnnoNum -> Element Msg
genNumberView blink color (Sem.AnnoNum num anno) =
    let
        newColor =
            Hi.lighten color
    in
    singletonHeader blink newColor (String.fromInt num) <| Just anno


genTokenView : Hi.Blink -> Sem.AnnoToken -> Element Msg
genTokenView blink (Sem.AnnoToken sym name anno) =
    singletonHeader blink
        theme.token
        (if sym == "" && name == "" then
            "Ada"

         else
            "Token with currency "
                ++ sym
                ++ "and name "
                ++ name
        )
    <|
        Just anno


genValueView : Hi.Blink -> Sem.AnnoValue Sem.AnnoObservation -> Element Msg
genValueView blink val =
    let
        color =
            theme.value
    in
    case val of
        Sem.AnnoAvailableMoney anno ->
            singletonHeader blink color "Available Money" <| Just anno

        Sem.AnnoConstant num anno ->
            [ topHeader blink color "Constant" <| Just anno
            , subScope <| genNumberView blink color num
            ]
                |> scopeBlock blink color

        Sem.AnnoNegValue v anno ->
            [ topHeader blink color "negative" <| Just anno
            , subScope <| genValueView blink v
            ]
                |> scopeBlock blink color

        Sem.AnnoAddValue v1 v2 anno ->
            [ subScope <| genValueView blink v1
            , midHeader blink color "plus" <| Just anno
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink color

        Sem.AnnoSubValue v1 v2 anno ->
            [ subScope <| genValueView blink v1
            , midHeader blink color "minus" <| Just anno
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink color

        Sem.AnnoMulValue v1 v2 anno ->
            [ subScope <| genValueView blink v1
            , midHeader blink color "times" <| Just anno
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink color

        Sem.AnnoScale rat v anno ->
            [ topHeader blink color "Scale by" <| Just anno
            , subScope <| genRationalView blink rat
            , midHeader blink color "the value" <| Just anno
            , subScope <| genValueView blink v
            ]
                |> scopeBlock blink color

        Sem.AnnoChoiceValue id anno ->
            [ topHeader blink color "Choice value" <| Just anno
            , subScope <| genChoiceIdView blink id
            ]
                |> scopeBlock blink color

        Sem.AnnoSlotIntervalStart anno ->
            singletonHeader blink color "Start of slot interval" <| Just anno

        Sem.AnnoSlotIntervalEnd anno ->
            singletonHeader blink color "End of slot interval" <| Just anno

        Sem.AnnoUseValue id anno ->
            [ topHeader blink color "Use value with identity" <| Just anno
            , subScope <| genNumberView blink color id
            ]
                |> scopeBlock blink color

        Sem.AnnoCond o a b anno ->
            [ topHeader blink color "If it's observed that" <| Just anno
            , subScope <| genObservationView blink o
            , midHeader blink color "then use the value" <| Just anno
            , subScope <| genValueView blink a
            , midHeader blink color "otherwise use" <| Just anno
            , subScope <| genValueView blink b
            ]
                |> scopeBlock blink color


genObservationView : Hi.Blink -> Sem.AnnoObservation -> Element Msg
genObservationView blink obs =
    let
        color =
            theme.observation
    in
    case obs of
        Sem.AnnoAndObs o1 o2 anno ->
            [ subScope <| genObservationView blink o1
            , midHeader blink color "and" <| Just anno
            , subScope <| genObservationView blink o2
            ]
                |> scopeBlock blink color

        Sem.AnnoOrObs o1 o2 anno ->
            [ subScope <| genObservationView blink o1
            , midHeader blink color "or" <| Just anno
            , subScope <| genObservationView blink o2
            ]
                |> scopeBlock blink color

        Sem.AnnoNotObs o anno ->
            [ topHeader blink color "not" <| Just anno
            , subScope <| genObservationView blink o
            ]
                |> scopeBlock blink color

        Sem.AnnoChooseSomething id anno ->
            [ topHeader blink color "This choice is made" <| Just anno
            , subScope <| genChoiceIdView blink id
            ]
                |> scopeBlock blink color

        Sem.AnnoValueGE v1 v2 anno ->
            [ subScope <| genValueView blink v1
            , midHeader blink color "is greater than or equal to" <| Just anno
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink color

        Sem.AnnoValueGT v1 v2 anno ->
            [ subScope <| genValueView blink v1
            , midHeader blink color "is greater than" <| Just anno
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink color

        Sem.AnnoValueLT v1 v2 anno ->
            [ subScope <| genValueView blink v1
            , midHeader blink color "is less than" <| Just anno
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink color

        Sem.AnnoValueLE v1 v2 anno ->
            [ subScope <| genValueView blink v1
            , midHeader blink color "is less than or equal to" <| Just anno
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink color

        Sem.AnnoValueEQ v1 v2 anno ->
            [ subScope <| genValueView blink v1
            , midHeader blink color "is equal to" <| Just anno
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink color

        Sem.AnnoTrueObs anno ->
            singletonHeader blink color "True" <| Just anno

        Sem.AnnoFalseObs anno ->
            singletonHeader blink color "False" <| Just anno


genActionView : Hi.Blink -> Sem.AnnoAction -> Element Msg
genActionView blink action =
    let
        color =
            theme.action
    in
    case action of
        Sem.AnnoDeposit getter giver tok val anno ->
            [ topHeader blink color "Deposit by" <| Just anno
            , subScope <| genPartyView blink giver
            , midHeader blink color "the amount of" <| Just anno
            , subScope <| genValueView blink val
            , midHeader blink color "currency" <| Just anno
            , subScope <| genTokenView blink tok
            , midHeader blink color "into account of" <| Just anno
            , subScope <| genPartyView blink getter
            ]
                |> scopeBlock blink color

        Sem.AnnoChoice choice bounds anno ->
            [ topHeader blink color "A choice made" <| Just anno
            , subScope <| genChoiceIdView blink choice
            , midHeader blink color "with bounds" <| Just anno
            , column
                [ paddingEach { edges | top = 15, left = 15, bottom = 15 }
                , spacing 15
                ]
                --(bounds |> List.map (genBoundsView blink))
                (bounds
                    |> List.map (genBoundsView blink)
                    |> (\x ->
                            (++) x
                                [ singletonHeader blink theme.bound "..." <| Just anno ]
                       )
                )

            --, subScope <| singletonHeader blink theme.bound "..." <| Just anno
            ]
                |> scopeBlock blink color

        Sem.AnnoNotify obs anno ->
            [ topHeader blink color "Notification of" <| Just anno
            , subScope <| genObservationView blink obs
            ]
                |> scopeBlock blink color


genBoundsView : Hi.Blink -> Sem.AnnoBound -> Element Msg
genBoundsView blink (Sem.AnnoBound a b anno) =
    let
        color =
            theme.bound
    in
    [ topHeader blink color "Between" <| Just anno
    , subScope <| genNumberView blink color a
    , midHeader blink color "and" <| Just anno
    , subScope <| genNumberView blink color b
    ]
        |> scopeBlock blink color


genChoiceIdView : Hi.Blink -> Sem.AnnoChoiceId -> Element Msg
genChoiceIdView blink (Sem.AnnoChoiceId choice owner anno) =
    let
        color =
            theme.value
    in
    [ topHeader blink color "Choice ID" <| Just anno
    , subScope <| genStringView blink color choice
    , midHeader blink color "owned by" <| Just anno
    , subScope <| genPartyView blink owner
    ]
        |> scopeBlock blink color


genRationalView : Hi.Blink -> Sem.AnnoRational -> Element Msg
genRationalView blink (Sem.AnnoRational n d anno) =
    let
        color =
            theme.value
    in
    [ subScope <| genNumberView blink color n
    , midHeader blink color "over" <| Just anno
    , subScope <| genNumberView blink color d
    ]
        |> scopeBlock blink theme.value



-- Managing View Complexity --


scopeBlock : Hi.Blink -> Color -> List (Element Msg) -> Element Msg
scopeBlock blink color elems =
    column
        [ Border.widthEach { edges | left = borderWidth }
        , Border.roundEach { corners | topLeft = margin, bottomLeft = margin }
        , Font.color <| Hi.blinkColor blink color
        ]
        elems


topHeader : Hi.Blink -> Color -> String -> Maybe Sem.Anno -> Element Msg
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
            , Font.color <| Hi.blinkColor blink color
            , Bg.color <| Hi.addAlpha 0.2 <| Hi.blinkColor blink color

            --, Bg.color Hi.bgBlue
            , Font.justify
            , paddingXY (gap * 2) gap
            ]
    in
    maybeButton style label maybeAnno


midHeader : Hi.Blink -> Color -> String -> Maybe Sem.Anno -> Element Msg
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
            , Font.color <| Hi.blinkColor blink color
            , Bg.color <| Hi.addAlpha 0.2 <| Hi.blinkColor blink color
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


singletonHeader : Hi.Blink -> Color -> String -> Maybe Sem.Anno -> Element Msg
singletonHeader blink color label maybeAnno =
    let
        style =
            [ Font.color <| Hi.blinkColor blink color
            , Border.width borderWidth
            , Border.rounded margin
            , Bg.color <| Hi.addAlpha 0.2 <| Hi.blinkColor blink color
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


keyboardButton : Hi.Blink -> Color -> String -> Element Msg
keyboardButton blink color label =
    el
        [ Font.color <| Hi.blinkColor blink color
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
