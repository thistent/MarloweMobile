module Marlowe.Semantics exposing (..)

import BigInt exposing (BigInt)
import Browser
import Browser.Dom as Dom
import Unique as U
import Url



-- Messages --


type Msg
    = NewUrl Url.Url
    | Link Browser.UrlRequest
    | TimeDelta Float
    | SwitchContract SampleContract
    | ToggleKeyboard
    | ToggleMenu
    | UpdateViewport Dom.Viewport
    | Select (Maybe Anno)


type SampleContract
    = CouponBond
    | Escrow
    | Swap
    | ZeroCoupon
    | NilContract



-- Marlowe Types --


type alias CurrencySymbol =
    String


type alias TokenName =
    String


type Token
    = Token CurrencySymbol TokenName


type AnnoToken
    = AnnoToken CurrencySymbol TokenName Anno


annotateToken : Token -> AnnoToken
annotateToken (Token sym name) =
    AnnoToken sym name U.unique


type alias ValueId =
    Int



-- Annotation --


type alias Anno =
    U.Unique U.Id



-- Contract --


type Contract
    = Close
    | Pay Party Payee Token (Value Observation) Contract
    | If Observation Contract Contract
    | When (List (Case Action Contract)) ValueId Contract
    | Let ValueId (Value Observation) Contract


type AnnoContract
    = AnnoClose Anno
    | AnnoPay AnnoParty AnnoPayee AnnoToken (AnnoValue AnnoObservation) AnnoContract Anno
    | AnnoIf AnnoObservation AnnoContract AnnoContract Anno
    | AnnoWhen (List (AnnoCase AnnoAction AnnoContract)) AnnoNum AnnoContract Anno
    | AnnoLet AnnoNum (AnnoValue AnnoObservation) AnnoContract Anno


annotateContract : Contract -> AnnoContract
annotateContract contract =
    case contract of
        Close ->
            AnnoClose U.unique

        Pay p1 p2 t v c ->
            AnnoPay
                (annotateParty p1)
                (annotatePayee p2)
                (annotateToken t)
                (annotateValue v)
                (annotateContract c)
                U.unique

        If o a b ->
            AnnoIf
                (annotateObservation o)
                (annotateContract a)
                (annotateContract b)
                U.unique

        When xs t y ->
            AnnoWhen
                (xs |> List.map annotateCase)
                (annotateNum t)
                (annotateContract y)
                U.unique

        Let id val a ->
            AnnoLet
                (annotateNum id)
                (annotateValue val)
                (annotateContract a)
                U.unique



-- Value --


type Value a
    = AvailableMoney --AccountId Token
    | Constant Int
    | NegValue (Value a)
    | AddValue (Value a) (Value a)
    | SubValue (Value a) (Value a)
    | MulValue (Value a) (Value a)
    | Scale Rational (Value a)
    | ChoiceValue ChoiceId -- (Value a) not present in Haskell code
    | SlotIntervalStart
    | SlotIntervalEnd
    | UseValue ValueId
    | Cond a (Value a) (Value a)


type AnnoValue a
    = AnnoAvailableMoney Anno
    | AnnoConstant AnnoNum Anno
    | AnnoNegValue (AnnoValue a) Anno
    | AnnoAddValue (AnnoValue a) (AnnoValue a) Anno
    | AnnoSubValue (AnnoValue a) (AnnoValue a) Anno
    | AnnoMulValue (AnnoValue a) (AnnoValue a) Anno
    | AnnoScale AnnoRational (AnnoValue a) Anno
    | AnnoChoiceValue AnnoChoiceId Anno
    | AnnoSlotIntervalStart Anno
    | AnnoSlotIntervalEnd Anno
    | AnnoUseValue AnnoNum Anno
    | AnnoCond a (AnnoValue a) (AnnoValue a) Anno


annotateValue : Value Observation -> AnnoValue AnnoObservation
annotateValue val =
    case val of
        AvailableMoney ->
            AnnoAvailableMoney U.unique

        Constant num ->
            AnnoConstant
                (annotateNum num)
                U.unique

        NegValue v ->
            AnnoNegValue
                (annotateValue v)
                U.unique

        AddValue v1 v2 ->
            AnnoAddValue
                (annotateValue v1)
                (annotateValue v2)
                U.unique

        SubValue v1 v2 ->
            AnnoSubValue
                (annotateValue v1)
                (annotateValue v2)
                U.unique

        MulValue v1 v2 ->
            AnnoMulValue
                (annotateValue v1)
                (annotateValue v2)
                U.unique

        Scale rat v ->
            AnnoScale
                (annotateRational rat)
                (annotateValue v)
                U.unique

        ChoiceValue id ->
            AnnoChoiceValue
                (annotateChoiceId id)
                U.unique

        SlotIntervalStart ->
            AnnoSlotIntervalStart U.unique

        SlotIntervalEnd ->
            AnnoSlotIntervalEnd U.unique

        UseValue id ->
            AnnoUseValue (annotateNum id) U.unique

        Cond o a b ->
            AnnoCond
                (annotateObservation o)
                (annotateValue a)
                (annotateValue b)
                U.unique



-- Observation --


type Observation
    = AndObs Observation Observation
    | OrObs Observation Observation
    | NotObs Observation
    | ChooseSomething ChoiceId
    | ValueGE (Value Observation) (Value Observation)
    | ValueGT (Value Observation) (Value Observation)
    | ValueLT (Value Observation) (Value Observation)
    | ValueLE (Value Observation) (Value Observation)
    | ValueEQ (Value Observation) (Value Observation)
    | TrueObs
    | FalseObs


type AnnoObservation
    = AnnoAndObs AnnoObservation AnnoObservation Anno
    | AnnoOrObs AnnoObservation AnnoObservation Anno
    | AnnoNotObs AnnoObservation Anno
    | AnnoChooseSomething AnnoChoiceId Anno
    | AnnoValueGE (AnnoValue AnnoObservation) (AnnoValue AnnoObservation) Anno
    | AnnoValueGT (AnnoValue AnnoObservation) (AnnoValue AnnoObservation) Anno
    | AnnoValueLT (AnnoValue AnnoObservation) (AnnoValue AnnoObservation) Anno
    | AnnoValueLE (AnnoValue AnnoObservation) (AnnoValue AnnoObservation) Anno
    | AnnoValueEQ (AnnoValue AnnoObservation) (AnnoValue AnnoObservation) Anno
    | AnnoTrueObs Anno
    | AnnoFalseObs Anno


annotateObservation : Observation -> AnnoObservation
annotateObservation obs =
    case obs of
        AndObs o1 o2 ->
            AnnoAndObs
                (annotateObservation o1)
                (annotateObservation o2)
                U.unique

        OrObs o1 o2 ->
            AnnoOrObs
                (annotateObservation o1)
                (annotateObservation o2)
                U.unique

        NotObs o ->
            AnnoNotObs
                (annotateObservation o)
                U.unique

        ChooseSomething id ->
            AnnoChooseSomething
                (annotateChoiceId id)
                U.unique

        ValueGE v1 v2 ->
            AnnoValueGE
                (annotateValue v1)
                (annotateValue v2)
                U.unique

        ValueGT v1 v2 ->
            AnnoValueGT
                (annotateValue v1)
                (annotateValue v2)
                U.unique

        ValueLT v1 v2 ->
            AnnoValueLT
                (annotateValue v1)
                (annotateValue v2)
                U.unique

        ValueLE v1 v2 ->
            AnnoValueLE
                (annotateValue v1)
                (annotateValue v2)
                U.unique

        ValueEQ v1 v2 ->
            AnnoValueEQ
                (annotateValue v1)
                (annotateValue v2)
                U.unique

        TrueObs ->
            AnnoTrueObs U.unique

        FalseObs ->
            AnnoFalseObs U.unique



-- Action --


type Action
    = Deposit Party Party Token (Value Observation)
    | Choice ChoiceId (List Bound)
    | Notify Observation


type AnnoAction
    = AnnoDeposit AnnoParty AnnoParty AnnoToken (AnnoValue AnnoObservation) Anno
    | AnnoChoice AnnoChoiceId (List AnnoBound) Anno
    | AnnoNotify AnnoObservation Anno


annotateAction : Action -> AnnoAction
annotateAction act =
    case act of
        Deposit p1 p2 tok v ->
            AnnoDeposit
                (annotateParty p1)
                (annotateParty p2)
                (annotateToken tok)
                (annotateValue v)
                U.unique

        Choice id xs ->
            AnnoChoice
                (annotateChoiceId id)
                --(annotateParty party)
                (xs |> List.map annotateBound)
                U.unique

        Notify o ->
            AnnoNotify
                (annotateObservation o)
                U.unique



-- Case --


type Case a b
    = Case a b


type AnnoCase a b
    = AnnoCase a b Anno


annotateCase : Case Action Contract -> AnnoCase AnnoAction AnnoContract
annotateCase (Case a c) =
    AnnoCase
        (annotateAction a)
        (annotateContract c)
        U.unique



-- Party --


type Party
    = PK String
    | Role String


type AnnoParty
    = AnnoPK AnnoString Anno
    | AnnoRole AnnoString Anno


annotateParty : Party -> AnnoParty
annotateParty p =
    case p of
        PK key ->
            AnnoPK (AnnoString key U.unique) U.unique

        Role name ->
            AnnoRole (AnnoString name U.unique) U.unique



-- Payee --


type Payee
    = Account AccountId
    | Party Party


type AnnoPayee
    = AnnoAccount AnnoAccountId Anno
    | AnnoParty AnnoParty Anno


annotatePayee : Payee -> AnnoPayee
annotatePayee p =
    case p of
        Account id ->
            AnnoAccount
                (annotateAccountId id)
                U.unique

        Party party ->
            AnnoParty
                (annotateParty party)
                U.unique



-- ChoiceId --


type ChoiceId
    = ChoiceId String Party


type AnnoChoiceId
    = AnnoChoiceId AnnoString AnnoParty Anno


annotateChoiceId : ChoiceId -> AnnoChoiceId
annotateChoiceId (ChoiceId s p) =
    AnnoChoiceId
        (annotateString s)
        (annotateParty p)
        U.unique



-- Rational --


type Rational
    = Rational Int Int


type AnnoRational
    = AnnoRational AnnoNum AnnoNum Anno


annotateRational : Rational -> AnnoRational
annotateRational (Rational n1 n2) =
    AnnoRational
        (annotateNum n1)
        (annotateNum n2)
        U.unique



-- AccountId --


type AccountId
    = AccountId Int String


type AnnoAccountId
    = AnnoAccountId AnnoNum AnnoString Anno


annotateAccountId : AccountId -> AnnoAccountId
annotateAccountId (AccountId num str) =
    AnnoAccountId
        (annotateNum num)
        (annotateString str)
        U.unique



-- Bound --


type Bound
    = Bound Int Int


type AnnoBound
    = AnnoBound AnnoNum AnnoNum Anno


annotateBound : Bound -> AnnoBound
annotateBound (Bound i j) =
    AnnoBound
        (annotateNum i)
        (annotateNum j)
        U.unique



-- Number --


type AnnoNum
    = AnnoNum Int Anno


annotateNum : Int -> AnnoNum
annotateNum n =
    AnnoNum n U.unique



-- String --


type AnnoString
    = AnnoString String Anno


annotateString : String -> AnnoString
annotateString s =
    AnnoString s U.unique



-- For holding different types of values in clipboard for pasting --


type Expr
    = StringExpr String
    | NumExpr Int
    | TokenExpr Token
    | ValueIdExpr ValueId
    | PayeeExpr Payee
    | BoundExpr Bound
    | CaseExpr (Case Action Contract)
    | RationalExpr Rational
    | AccountIdExpr AccountId
    | ChoiceIdExpr ChoiceId
    | ContractExpr Contract
    | ValueExpr (Value Observation)
    | ObservationExpr Observation
    | ActionExpr Action



-- Unique Tree Testing --


type Tree a
    = Branch (Tree a) (Tree a)
    | Leaf a U.Id


type alias TreeGen a =
    U.Unique (Tree a)


leaf : a -> TreeGen a
leaf a =
    U.map (Leaf a) U.unique


branch : TreeGen a -> TreeGen a -> TreeGen a
branch left right =
    U.map2 Branch left right


splice : U.Id -> TreeGen a -> Tree a -> TreeGen a
splice id graft tree =
    case tree of
        Leaf x k ->
            if k == id then
                graft

            else
                leaf x

        Branch left right ->
            branch (splice id graft left) (splice id graft right)



-- Sample Contracts --


couponBondGuaranteed : Contract
couponBondGuaranteed =
    When
        [ Case
            (Deposit
                (Role "investor")
                (Role "investor")
                (Token "" "")
                (Constant 850)
            )
            (Pay
                (Role "investor")
                (Party
                    (Role "issuer")
                )
                (Token "" "")
                (Constant 850)
                (When
                    [ Case
                        (Deposit
                            (Role "investor")
                            (Role "issuer")
                            (Token "" "")
                            (Constant 1000)
                        )
                        (Pay
                            (Role "investor")
                            (Party
                                (Role "investor")
                            )
                            (Token "" "")
                            (Constant 1000)
                            Close
                        )
                    ]
                    20
                    Close
                )
            )
        ]
        10
        Close


escrow : Contract
escrow =
    When
        [ Case
            (Deposit
                (Role "alice")
                (Role "alice")
                (Token "" "")
                (Constant 450)
            )
            (When
                [ Case
                    (Choice
                        (ChoiceId "choice"
                            (Role "alice")
                        )
                        [ Bound 0 1
                        ]
                    )
                    (When
                        [ Case
                            (Choice
                                (ChoiceId "choice"
                                    (Role "bob")
                                )
                                [ Bound 0 1
                                ]
                            )
                            (If
                                (ValueEQ
                                    (ChoiceValue
                                        (ChoiceId "choice"
                                            (Role "alice")
                                        )
                                    )
                                    (ChoiceValue
                                        (ChoiceId "choice"
                                            (Role "bob")
                                        )
                                    )
                                )
                                (If
                                    (ValueEQ
                                        (ChoiceValue
                                            (ChoiceId "choice"
                                                (Role "alice")
                                            )
                                        )
                                        (Constant 0)
                                    )
                                    (Pay
                                        (Role "alice")
                                        (Party
                                            (Role "bob")
                                        )
                                        (Token "" "")
                                        (Constant 450)
                                        Close
                                    )
                                    Close
                                )
                                (When
                                    [ Case
                                        (Choice
                                            (ChoiceId "choice"
                                                (Role "carol")
                                            )
                                            [ Bound 1 1
                                            ]
                                        )
                                        Close
                                    , Case
                                        (Choice
                                            (ChoiceId "choice"
                                                (Role "carol")
                                            )
                                            [ Bound 0 0
                                            ]
                                        )
                                        (Pay
                                            (Role "alice")
                                            (Party
                                                (Role "bob")
                                            )
                                            (Token "" "")
                                            (Constant 450)
                                            Close
                                        )
                                    ]
                                    100
                                    Close
                                )
                            )
                        ]
                        60
                        (When
                            [ Case
                                (Choice
                                    (ChoiceId "choice"
                                        (Role "carol")
                                    )
                                    [ Bound 1 1
                                    ]
                                )
                                Close
                            , Case
                                (Choice
                                    (ChoiceId "choice"
                                        (Role "carol")
                                    )
                                    [ Bound 0 0
                                    ]
                                )
                                (Pay
                                    (Role "alice")
                                    (Party
                                        (Role "bob")
                                    )
                                    (Token "" "")
                                    (Constant 450)
                                    Close
                                )
                            ]
                            100
                            Close
                        )
                    )
                ]
                40
                (When
                    [ Case
                        (Choice
                            (ChoiceId "choice"
                                (Role "carol")
                            )
                            [ Bound 1 1
                            ]
                        )
                        Close
                    , Case
                        (Choice
                            (ChoiceId "choice"
                                (Role "carol")
                            )
                            [ Bound 0 0
                            ]
                        )
                        (Pay
                            (Role "alice")
                            (Party
                                (Role "bob")
                            )
                            (Token "" "")
                            (Constant 450)
                            Close
                        )
                    ]
                    100
                    Close
                )
            )
        ]
        10
        Close


swap : Contract
swap =
    When
        [ Case
            (Deposit
                (Role "party1")
                (Role "party1")
                (Token "" "")
                (Constant 500)
            )
            (When
                [ Case
                    (Deposit
                        (Role "party2")
                        (Role "party2")
                        (Token "" "")
                        (Constant 300)
                    )
                    (Pay
                        (Role "party1")
                        (Party
                            (Role "party2")
                        )
                        (Token "" "")
                        (Constant 500)
                        (Pay
                            (Role "party2")
                            (Party
                                (Role "party1")
                            )
                            (Token "" "")
                            (Constant 300)
                            Close
                        )
                    )
                ]
                20
                Close
            )
        ]
        15
        Close


zeroCouponBond : Contract
zeroCouponBond =
    When
        [ Case
            (Deposit
                (Role "investor")
                (Role "investor")
                (Token "" "")
                (Constant 850)
            )
            (Pay
                (Role "investor")
                (Party
                    (Role "issuer")
                )
                (Token "" "")
                (Constant 850)
                (When
                    [ Case
                        (Deposit
                            (Role "investor")
                            (Role "issuer")
                            (Token "" "")
                            (Constant 1000)
                        )
                        (Pay
                            (Role "investor")
                            (Party
                                (Role "investor")
                            )
                            (Token "" "")
                            (Constant 1000)
                            Close
                        )
                    ]
                    20
                    Close
                )
            )
        ]
        10
        Close
