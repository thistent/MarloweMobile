module Main exposing (..)

import Browser
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
    }


type Msg
    = NewUrl Url.Url
    | Link Browser.UrlRequest
    | TimeDelta Float
    | SwitchContract SampleContract
    | ToggleKeyboard


type SampleContract
    = CouponBond
    | Escrow
    | Swap
    | ZeroCoupon


type Blink
    = Regular
    | Half
    | Full



-- Initialization --


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url 0.0 Regular Sem.escrow False, Cmd.none )



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
                ( { model | timeDelta = 0 }, Cmd.none )

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

        ToggleKeyboard ->
            ( { model | keyboardState = not model.keyboardState }
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
            , Bg.color Hi.bgBlue
            , Font.color Hi.accentPink
            , Font.size 30
            , Font.family
                [ Font.typeface "Helvetica"
                , Font.sansSerif
                ]
            , inFront <|
                if model.keyboardState then
                    Kb.kb

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
                     , subScope <| genContractView model.blink model.sampleContract
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
            singletonHeader blink Hi.refund "Refund"

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

        Sem.When xs t y ->
            [ topHeader "When"
            , column
                [ paddingEach { edges | top = margin, left = margin, bottom = margin }
                ]
                (xs |> List.map (genCaseView blink))
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
    [ topHeader "In case of action"
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
                |> scopeBlock blink (rgb 0.32 0.4 0.8)

        Sem.ChooseSomething id ->
            [ topHeader "The choice"
            , subScope <| genChoiceIdView blink id
            ]
                |> scopeBlock blink (rgb 0.32 0.4 0.8)

        Sem.ValueGE v1 v2 ->
            [ subScope <| genValueView blink v1
            , midHeader "is greater than or equal to"
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink (rgb 0.4 0.5 1)

        Sem.ValueGT v1 v2 ->
            [ subScope <| genValueView blink v1
            , midHeader "is greater than"
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink (rgb 0.4 0.5 1)

        Sem.ValueLT v1 v2 ->
            [ subScope <| genValueView blink v1
            , midHeader "is less than"
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink (rgb 0.4 0.5 1)

        Sem.ValueLE v1 v2 ->
            [ subScope <| genValueView blink v1
            , midHeader "is less than or equal to"
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink (rgb 0.4 0.5 1)

        Sem.ValueEQ v1 v2 ->
            [ subScope <| genValueView blink v1
            , midHeader "is equal to"
            , subScope <| genValueView blink v2
            ]
                |> scopeBlock blink (rgb 0.4 0.5 1)

        Sem.TrueObs ->
            singletonHeader blink (rgb 0.64 0.8 1) "True"

        Sem.FalseObs ->
            singletonHeader blink (rgb 0.2 0.25 0.5) "False"


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
                |> scopeBlock blink (rgb 0.6 0.3 0.7)

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
            ]
                |> scopeBlock blink (rgb 0.6 0.3 0.7)

        Sem.Notify obs ->
            [ topHeader "Notify when"
            , subScope <| genObservationView blink obs
            ]
                |> scopeBlock blink (rgb 0.6 0.3 0.7)


genBoundsView : Blink -> Sem.Bound -> Element Msg
genBoundsView blink (Sem.Bound a b) =
    [ topHeader "Between"
    , subScope <| genNumberView blink a
    , midHeader "and"
    , subScope <| genNumberView blink b
    ]
        |> scopeBlock blink (rgb 0.6 0.3 0.15)


genChoiceIdView : Blink -> Sem.ChoiceId -> Element Msg
genChoiceIdView blink (Sem.ChoiceId choice owner) =
    [ topHeader "A value identified by"
    , subScope <| genStringView blink choice
    , midHeader "with owner"
    , subScope <| genStringView blink owner
    ]
        |> scopeBlock blink (rgb 0.3 0.6 0.15)


genRationalView : Blink -> Sem.Rational -> Element Msg
genRationalView blink (Sem.Rational n d) =
    [ subScope <| genNumberView blink n
    , midHeader "over"
    , subScope <| genNumberView blink d
    ]
        |> scopeBlock blink (rgb 0.6 0.8 0.2)



-- Mini View for Clipboard --


genMiniContractView : Blink -> Sem.Contract -> Element Msg
genMiniContractView blink contract =
    case contract of
        Sem.Refund ->
            singletonMiniHeader blink (rgb 0.65 0.05 0) ""

        Sem.Pay p1 p2 v a ->
            [ topMiniHeader ""
            , subMiniScope <| genMiniAccountIdView blink p1
            , midMiniHeader ""
            , subMiniScope <| genMiniPayeeView blink p2
            , midMiniHeader ""
            , subMiniScope <| genMiniValueView blink v
            , midMiniHeader ""
            , subMiniScope <| genMiniContractView blink a
            ]
                |> scopeMiniBlock blink (rgb 1 0.55 0.15)

        Sem.If o a b ->
            [ topMiniHeader ""
            , subMiniScope <| genMiniObservationView blink o
            , midMiniHeader ""
            , subMiniScope <| genMiniContractView blink a
            , midMiniHeader ""
            , subMiniScope <| genMiniContractView blink b
            ]
                |> scopeMiniBlock blink (rgb 1 0.4 0)

        Sem.When xs t y ->
            [ topMiniHeader ""
            , column
                [ paddingEach { edges | top = 1, left = 1, bottom = 1 }
                ]
                (xs |> List.map (genMiniCaseView blink))
            , midMiniHeader ""
            , subMiniScope <| genMiniNumberView blink t
            , midMiniHeader ""
            , subMiniScope <| genMiniContractView blink y
            ]
                |> scopeMiniBlock blink Hi.contractColor

        Sem.Let id val a ->
            [ topMiniHeader ""
            , subMiniScope <| genMiniNumberView blink id
            , midMiniHeader ""
            , subMiniScope <| genMiniValueView blink val
            , midMiniHeader ""
            , subMiniScope <| genMiniContractView blink a
            ]
                |> scopeMiniBlock blink (rgb 0.8 0.2 0.4)


genMiniCaseView : Blink -> Sem.Case Sem.Action Sem.Contract -> Element Msg
genMiniCaseView blink (Sem.Case a c) =
    [ topMiniHeader ""
    , subMiniScope <| genMiniActionView blink a
    , midMiniHeader ""
    , subMiniScope <| genMiniContractView blink c
    ]
        |> scopeMiniBlock blink (rgb 1 0.8 0)


genMiniPayeeView : Blink -> Sem.Payee -> Element Msg
genMiniPayeeView blink payee =
    case payee of
        Sem.Account id ->
            genMiniAccountIdView blink id

        Sem.Party name ->
            genMiniStringView blink name


genMiniAccountIdView : Blink -> Sem.AccountId -> Element Msg
genMiniAccountIdView blink (Sem.AccountId num owner) =
    [ topMiniHeader ""
    , subMiniScope <| genMiniNumberView blink num
    , midMiniHeader ""
    , subMiniScope <| genMiniStringView blink owner
    ]
        |> scopeMiniBlock blink (rgb 0 0.45 0.65)


genMiniStringView : Blink -> String -> Element Msg
genMiniStringView blink str =
    singletonMiniHeader blink (rgb 0 0.5 0.2) ""


genMiniNumberView : Blink -> Int -> Element Msg
genMiniNumberView blink num =
    singletonMiniHeader blink (rgb 0.6 0.8 0.2) ""


genMiniValueView : Blink -> Sem.Value Sem.Observation -> Element Msg
genMiniValueView blink val =
    case val of
        Sem.AvailableMoney ->
            singletonMiniHeader blink (rgb 0.6 0.8 0.2) <| ""

        Sem.Constant num ->
            singletonMiniHeader blink (rgb 0.6 0.8 0.2) <| ""

        Sem.NegValue v ->
            [ topMiniHeader ""
            , subMiniScope <| genMiniValueView blink v
            ]
                |> scopeMiniBlock blink (rgb 0.6 0.8 0.2)

        Sem.AddValue v1 v2 ->
            [ subMiniScope <| genMiniValueView blink v1
            , midMiniHeader ""
            , subMiniScope <| genMiniValueView blink v2
            ]
                |> scopeMiniBlock blink (rgb 0.6 0.8 0.2)

        Sem.SubValue v1 v2 ->
            [ subMiniScope <| genMiniValueView blink v1
            , midMiniHeader ""
            , subMiniScope <| genMiniValueView blink v2
            ]
                |> scopeMiniBlock blink (rgb 0.6 0.8 0.2)

        Sem.MulValue v1 v2 ->
            [ subMiniScope <| genMiniValueView blink v1
            , midMiniHeader ""
            , subMiniScope <| genMiniValueView blink v2
            ]
                |> scopeMiniBlock blink (rgb 0.6 0.8 0.2)

        Sem.Scale rat v ->
            [ topMiniHeader ""
            , subMiniScope <| genMiniRationalView blink rat
            , midMiniHeader ""
            , subMiniScope <| genMiniValueView blink v
            ]
                |> scopeMiniBlock blink
                    (rgb 0.6 0.8 0.2)

        Sem.ChoiceValue id v ->
            [ topMiniHeader ""
            , subMiniScope <| genMiniChoiceIdView blink id
            , midMiniHeader ""
            , subMiniScope <| genMiniValueView blink v
            , endMiniHeader ""
            ]
                |> scopeMiniBlock blink
                    (rgb 0.6 0.8 0.2)

        Sem.SlotIntervalStart ->
            singletonMiniHeader blink (rgb 0.6 0.8 0.2) ""

        Sem.SlotIntervalEnd ->
            singletonMiniHeader blink (rgb 0.6 0.8 0.2) ""

        Sem.UseValue id ->
            [ topMiniHeader ""
            , subMiniScope <| genMiniNumberView blink id
            ]
                |> scopeMiniBlock blink (rgb 0.6 0.8 0.2)

        Sem.Cond o a b ->
            [ topMiniHeader ""
            , subMiniScope <| genMiniObservationView blink o
            , midMiniHeader ""
            , subMiniScope <| genMiniValueView blink a
            , midMiniHeader ""
            , subMiniScope <| genMiniValueView blink b
            ]
                |> scopeMiniBlock blink
                    (rgb 0.6 0.8 0.2)


genMiniObservationView : Blink -> Sem.Observation -> Element Msg
genMiniObservationView blink obs =
    case obs of
        Sem.AndObs o1 o2 ->
            [ subMiniScope <| genMiniObservationView blink o1
            , midMiniHeader ""
            , subMiniScope <| genMiniObservationView blink o2
            ]
                |> scopeMiniBlock blink (rgb 0.4 0.5 1)

        Sem.OrObs o1 o2 ->
            [ subMiniScope <| genMiniObservationView blink o1
            , midMiniHeader ""
            , subMiniScope <| genMiniObservationView blink o2
            ]
                |> scopeMiniBlock blink (rgb 0.4 0.5 1)

        Sem.NotObs o ->
            [ topMiniHeader ""
            , subMiniScope <| genMiniObservationView blink o
            ]
                |> scopeMiniBlock blink (rgb 0.32 0.4 0.8)

        Sem.ChooseSomething id ->
            [ topMiniHeader ""
            , subMiniScope <| genMiniChoiceIdView blink id
            ]
                |> scopeMiniBlock blink (rgb 0.32 0.4 0.8)

        Sem.ValueGE v1 v2 ->
            [ subMiniScope <| genMiniValueView blink v1
            , midMiniHeader ""
            , subMiniScope <| genMiniValueView blink v2
            ]
                |> scopeMiniBlock blink (rgb 0.4 0.5 1)

        Sem.ValueGT v1 v2 ->
            [ subMiniScope <| genMiniValueView blink v1
            , midMiniHeader ""
            , subMiniScope <| genMiniValueView blink v2
            ]
                |> scopeMiniBlock blink (rgb 0.4 0.5 1)

        Sem.ValueLT v1 v2 ->
            [ subMiniScope <| genMiniValueView blink v1
            , midMiniHeader ""
            , subMiniScope <| genMiniValueView blink v2
            ]
                |> scopeMiniBlock blink (rgb 0.4 0.5 1)

        Sem.ValueLE v1 v2 ->
            [ subMiniScope <| genMiniValueView blink v1
            , midMiniHeader ""
            , subMiniScope <| genMiniValueView blink v2
            ]
                |> scopeMiniBlock blink (rgb 0.4 0.5 1)

        Sem.ValueEQ v1 v2 ->
            [ subMiniScope <| genMiniValueView blink v1
            , midMiniHeader ""
            , subMiniScope <| genMiniValueView blink v2
            ]
                |> scopeMiniBlock blink (rgb 0.4 0.5 1)

        Sem.TrueObs ->
            singletonMiniHeader blink (rgb 0.64 0.8 1) ""

        Sem.FalseObs ->
            singletonMiniHeader blink (rgb 0.2 0.25 0.5) ""


genMiniActionView : Blink -> Sem.Action -> Element Msg
genMiniActionView blink action =
    case action of
        Sem.Deposit id str val ->
            [ topMiniHeader ""
            , subMiniScope <| genMiniAccountIdView blink id
            , midMiniHeader ""
            , subMiniScope <| genMiniStringView blink str
            , midMiniHeader ""
            , subMiniScope <| genMiniValueView blink val
            ]
                |> scopeMiniBlock blink (rgb 0.6 0.3 0.7)

        Sem.Choice (Sem.ChoiceId name owner) bounds ->
            [ topMiniHeader ""
            , subMiniScope <| genMiniStringView blink name
            , midMiniHeader ""
            , subMiniScope <| genMiniStringView blink owner
            , midMiniHeader ""
            , column
                [ paddingEach { edges | top = 1, left = 1, bottom = 1 }
                ]
                (bounds |> List.map (genMiniBoundsView blink))
            ]
                |> scopeMiniBlock blink (rgb 0.6 0.3 0.7)

        Sem.Notify obs ->
            [ topMiniHeader ""
            , subMiniScope <| genMiniObservationView blink obs
            ]
                |> scopeMiniBlock blink (rgb 0.6 0.3 0.7)


genMiniBoundsView : Blink -> Sem.Bound -> Element Msg
genMiniBoundsView blink (Sem.Bound a b) =
    [ topMiniHeader ""
    , subMiniScope <| genMiniNumberView blink a
    , midMiniHeader ""
    , subMiniScope <| genMiniNumberView blink b
    ]
        |> scopeMiniBlock blink (rgb 0.6 0.3 0.15)


genMiniChoiceIdView : Blink -> Sem.ChoiceId -> Element Msg
genMiniChoiceIdView blink (Sem.ChoiceId choice owner) =
    [ topMiniHeader ""
    , subMiniScope <| genMiniStringView blink choice
    , midMiniHeader ""
    , subMiniScope <| genMiniStringView blink owner
    ]
        |> scopeMiniBlock blink (rgb 0.3 0.6 0.15)


genMiniRationalView : Blink -> Sem.Rational -> Element Msg
genMiniRationalView blink (Sem.Rational n d) =
    [ subMiniScope <| genMiniNumberView blink n
    , midMiniHeader ""
    , subMiniScope <| genMiniNumberView blink d
    ]
        |> scopeMiniBlock blink (rgb 0.6 0.8 0.2)



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



-- Managing Mini Complexity --


scopeMiniBlock : Blink -> Color -> List (Element Msg) -> Element Msg
scopeMiniBlock blink color elems =
    column
        [ Border.widthEach { edges | left = borderWidth // 2 }
        , Border.roundEach
            { corners
                | topLeft = borderWidth
                , bottomLeft = borderWidth
            }
        , Font.color <| blinkColor blink color
        ]
        elems


topMiniHeader : String -> Element Msg
topMiniHeader label =
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
        text label


midMiniHeader : String -> Element Msg
midMiniHeader label =
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
        text label


endMiniHeader : String -> Element Msg
endMiniHeader label =
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
        text label


singletonMiniHeader : Blink -> Color -> String -> Element Msg
singletonMiniHeader blink color label =
    el
        [ Font.color <| blinkColor blink color
        , Border.width <| borderWidth // 2
        , Border.rounded borderWidth
        , paddingXY borderWidth (borderWidth // 2)
        ]
    <|
        text label


subMiniScope : Element Msg -> Element Msg
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
