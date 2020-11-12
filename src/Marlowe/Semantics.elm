module Marlowe.Semantics exposing (..)

import BigInt exposing (BigInt)
import Unique as U



-- Marlowe Types --


type alias CurrencySymbol =
    String


type alias TokenName =
    String


type Token
    = Token CurrencySymbol TokenName


type alias ValueId =
    Int


type Payee
    = Account AccountId
    | Party String


type Bound
    = Bound Int Int


type Case a b
    = Case a b


type Rational
    = Rational Int Int


type AccountId
    = AccountId Int String


type ChoiceId
    = ChoiceId String String


type Contract
    = Refund
    | Pay AccountId Payee (Value Observation) Contract
    | If Observation Contract Contract
    | When (List (Case Action Contract)) ValueId Contract
    | Let ValueId (Value Observation) Contract


type Value a
    = AvailableMoney --AccountId Token
    | Constant Int
    | NegValue (Value a)
    | AddValue (Value a) (Value a)
    | SubValue (Value a) (Value a)
    | MulValue (Value a) (Value a)
    | Scale Rational (Value a)
    | ChoiceValue ChoiceId (Value a) -- (Value a) not present in Haskell code
    | SlotIntervalStart
    | SlotIntervalEnd
    | UseValue ValueId
    | Cond a (Value a) (Value a)


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


type Action
    = Deposit AccountId String (Value Observation)
    | Choice ChoiceId (List Bound)
    | Notify Observation



-- Annotated Types --


type alias Anno =
    U.Unique U.Id



{-

   type alias AnnotatedContract =
       ( Contract, Annotation )


   type alias AnnotatedValue a =
       ( Value a, Annotation )


   type alias AnnotatedObservation =
       ( Observation, Annotation )


   type alias AnnotatedAction =
       ( Action, Annotation )
-}


type AnnoPayee
    = AnnoAccount AnnoAccountId Anno
    | AnnoParty String Anno


type AnnoBound
    = AnnoBound Int Int Anno


type AnnoRational
    = AnnoRational Int Int Anno


type AnnoAccountId
    = AnnoAccountId Int String Anno


type AnnoChoiceId
    = AnnoChoiceId String String Anno


type AnnoCase a b
    = AnnoCase a b Anno


type AnnoContract
    = AnnoRefund Anno
    | AnnoPay AnnoAccountId AnnoPayee (AnnoValue AnnoObservation) AnnoContract Anno
    | AnnoIf AnnoObservation AnnoContract AnnoContract Anno
    | AnnoWhen (List (AnnoCase AnnoAction AnnoContract)) Int AnnoContract Anno
    | AnnoLet (AnnoValue AnnoObservation) AnnoContract Anno


type AnnoValue a
    = AnnoAvailableMoney Anno
    | AnnoConstant Int Anno
    | AnnoNegValue (AnnoValue a) Anno
    | AnnoAddValue (AnnoValue a) (AnnoValue a) Anno
    | AnnoSubValue (AnnoValue a) (AnnoValue a) Anno
    | AnnoMulValue (AnnoValue a) (AnnoValue a) Anno
    | AnnoScale AnnoRational (AnnoValue a) Anno
    | AnnoChoiceValue AnnoChoiceId (AnnoValue a) Anno
    | AnnoSlotIntervalStart Anno
    | AnnoSlotIntervalEnd Anno
    | AnnoUseValue Int Anno
    | AnnoCond a (AnnoValue a) (AnnoValue a) Anno


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


type AnnoAction
    = AnnoDeposit AnnoAccountId String (AnnoValue AnnoObservation) Anno
    | AnnoChoice AnnoChoiceId (List AnnoBound) Anno
    | AnnoNotify AnnoObservation Anno



-- Annotation Functions --


annotateContract : Contract -> AnnoContract
annotateContract contract =
    case contract of
        Refund ->
            AnnoRefund U.unique

        Pay p1 p2 v c ->
            AnnoPay
                (annotateAccountId p1)
                (annotatePayee p2)
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
                t
                (annotateContract y)
                U.unique

        Let id val a ->
            AnnoRefund U.unique


annotateValue : Value Observation -> AnnoValue AnnoObservation
annotateValue val =
    case val of
        AvailableMoney ->
            AnnoAvailableMoney U.unique

        Constant num ->
            AnnoConstant num U.unique

        NegValue v ->
            AnnoNegValue (annotateValue v) U.unique

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

        ChoiceValue id v ->
            AnnoAvailableMoney U.unique

        SlotIntervalStart ->
            AnnoAvailableMoney U.unique

        SlotIntervalEnd ->
            AnnoAvailableMoney U.unique

        UseValue id ->
            AnnoAvailableMoney U.unique

        Cond o a b ->
            AnnoAvailableMoney U.unique


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


annotateAction : Action -> AnnoAction
annotateAction act =
    case act of
        Deposit id str v ->
            AnnoDeposit (annotateAccountId id)
                str
                (annotateValue v)
                U.unique

        Choice id xs ->
            AnnoChoice (annotateChoiceId id)
                (xs |> List.map annotateBound)
                U.unique

        Notify o ->
            AnnoNotify (annotateObservation o) U.unique


annotateCase : Case Action Contract -> AnnoCase AnnoAction AnnoContract
annotateCase (Case a c) =
    AnnoCase (annotateAction a)
        (annotateContract c)
        U.unique


annotateChoiceId : ChoiceId -> AnnoChoiceId
annotateChoiceId (ChoiceId s1 s2) =
    AnnoChoiceId s1 s2 U.unique


annotateRational : Rational -> AnnoRational
annotateRational (Rational n1 n2) =
    AnnoRational n1 n2 U.unique


annotateAccountId : AccountId -> AnnoAccountId
annotateAccountId (AccountId num str) =
    AnnoAccountId num str U.unique


annotatePayee : Payee -> AnnoPayee
annotatePayee p =
    case p of
        Account id ->
            AnnoAccount
                (annotateAccountId id)
                U.unique

        Party str ->
            AnnoParty str U.unique


annotateBound : Bound -> AnnoBound
annotateBound (Bound i j) =
    AnnoBound i j U.unique



{-
   annotateAction : Action -> AnnoAction
   annotateAction act =
       case act of
           Deposit id str val ->
               AnnoDeposit (annotateAccountId id) String (AnnoValue AnnoObservation) Anno

           Choice (ChoiceId name owner) bounds ->
               AnnoChoice AnnoChoiceId (List AnnoBound) Anno

           Notify obs ->
               AnnoNotify AnnoObservation Anno
-}
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
                (AccountId 0 "investor")
                "guarantor"
                (Constant 1030)
            )
            (When
                [ Case
                    (Deposit
                        (AccountId 0 "investor")
                        "investor"
                        (Constant 1000)
                    )
                    (Pay
                        (AccountId 0 "investor")
                        (Party "issuer")
                        (Constant 1000)
                        (When
                            [ Case
                                (Deposit
                                    (AccountId 0 "investor")
                                    "issuer"
                                    (Constant 10)
                                )
                                (Pay
                                    (AccountId 0 "investor")
                                    (Party "investor")
                                    (Constant 10)
                                    (Pay
                                        (AccountId 0 "investor")
                                        (Party "guarantor")
                                        (Constant 10)
                                        (When
                                            [ Case
                                                (Deposit
                                                    (AccountId 0 "investor")
                                                    "issuer"
                                                    (Constant 10)
                                                )
                                                (Pay
                                                    (AccountId 0 "investor")
                                                    (Party "investor")
                                                    (Constant 10)
                                                    (Pay
                                                        (AccountId 0 "investor")
                                                        (Party "guarantor")
                                                        (Constant 10)
                                                        (When
                                                            [ Case
                                                                (Deposit
                                                                    (AccountId 0 "investor")
                                                                    "issuer"
                                                                    (Constant 1010)
                                                                )
                                                                (Pay
                                                                    (AccountId 0 "investor")
                                                                    (Party "investor")
                                                                    (Constant 1010)
                                                                    (Pay
                                                                        (AccountId 0 "investor")
                                                                        (Party "guarantor")
                                                                        (Constant 1010)
                                                                        Refund
                                                                    )
                                                                )
                                                            ]
                                                            20
                                                            Refund
                                                        )
                                                    )
                                                )
                                            ]
                                            15
                                            Refund
                                        )
                                    )
                                )
                            ]
                            10
                            Refund
                        )
                    )
                ]
                5
                Refund
            )
        ]
        5
        Refund


escrow : Contract
escrow =
    When
        [ Case
            (Deposit
                (AccountId 0 "alice")
                "alice"
                (Constant 450)
            )
            (When
                [ Case
                    (Choice
                        (ChoiceId "choice" "alice")
                        [ Bound 0 1
                        ]
                    )
                    (When
                        [ Case
                            (Choice
                                (ChoiceId "choice" "bob")
                                [ Bound 0 1
                                ]
                            )
                            (If
                                (ValueEQ
                                    (ChoiceValue
                                        (ChoiceId "choice" "alice")
                                        (Constant 42)
                                    )
                                    (ChoiceValue
                                        (ChoiceId "choice" "bob")
                                        (Constant 42)
                                    )
                                )
                                (If
                                    (ValueEQ
                                        (ChoiceValue
                                            (ChoiceId "choice" "alice")
                                            (Constant 42)
                                        )
                                        (Constant 0)
                                    )
                                    (Pay
                                        (AccountId 0 "alice")
                                        (Party "bob")
                                        (Constant 450)
                                        Refund
                                    )
                                    Refund
                                )
                                (When
                                    [ Case
                                        (Choice
                                            (ChoiceId "choice" "carol")
                                            [ Bound 1 1
                                            ]
                                        )
                                        Refund
                                    , Case
                                        (Choice
                                            (ChoiceId "choice" "carol")
                                            [ Bound 0 0
                                            ]
                                        )
                                        (Pay
                                            (AccountId 0 "alice")
                                            (Party "bob")
                                            (Constant 450)
                                            Refund
                                        )
                                    ]
                                    100
                                    Refund
                                )
                            )
                        ]
                        60
                        (When
                            [ Case
                                (Choice
                                    (ChoiceId "choice" "carol")
                                    [ Bound 1 1
                                    ]
                                )
                                Refund
                            , Case
                                (Choice
                                    (ChoiceId "choice" "carol")
                                    [ Bound 0 0
                                    ]
                                )
                                (Pay
                                    (AccountId 0 "alice")
                                    (Party "bob")
                                    (Constant 450)
                                    Refund
                                )
                            ]
                            100
                            Refund
                        )
                    )
                , Case
                    (Choice
                        (ChoiceId "choice" "bob")
                        [ Bound 0 1
                        ]
                    )
                    (When
                        [ Case
                            (Choice
                                (ChoiceId "choice" "alice")
                                [ Bound 0 1
                                ]
                            )
                            (If
                                (ValueEQ
                                    (ChoiceValue
                                        (ChoiceId "choice" "alice")
                                        (Constant 42)
                                    )
                                    (ChoiceValue
                                        (ChoiceId "choice" "bob")
                                        (Constant 42)
                                    )
                                )
                                (If
                                    (ValueEQ
                                        (ChoiceValue
                                            (ChoiceId "choice" "alice")
                                            (Constant 42)
                                        )
                                        (Constant 0)
                                    )
                                    (Pay
                                        (AccountId 0 "alice")
                                        (Party "bob")
                                        (Constant 450)
                                        Refund
                                    )
                                    Refund
                                )
                                (When
                                    [ Case
                                        (Choice
                                            (ChoiceId "choice" "carol")
                                            [ Bound 1 1
                                            ]
                                        )
                                        Refund
                                    , Case
                                        (Choice
                                            (ChoiceId "choice" "carol")
                                            [ Bound 0 0
                                            ]
                                        )
                                        (Pay
                                            (AccountId 0 "alice")
                                            (Party "bob")
                                            (Constant 450)
                                            Refund
                                        )
                                    ]
                                    100
                                    Refund
                                )
                            )
                        ]
                        60
                        (When
                            [ Case
                                (Choice
                                    (ChoiceId "choice" "carol")
                                    [ Bound 1 1
                                    ]
                                )
                                Refund
                            , Case
                                (Choice
                                    (ChoiceId "choice" "carol")
                                    [ Bound 0 0
                                    ]
                                )
                                (Pay
                                    (AccountId 0 "alice")
                                    (Party "bob")
                                    (Constant 450)
                                    Refund
                                )
                            ]
                            100
                            Refund
                        )
                    )
                ]
                40
                Refund
            )
        ]
        10
        Refund


swap : Contract
swap =
    When
        [ Case
            (Deposit
                (AccountId 1 "party1")
                "party1"
                (Constant 500)
            )
            (When
                [ Case
                    (Deposit
                        (AccountId 2 "party2")
                        "party2"
                        (Constant 300)
                    )
                    (Pay
                        (AccountId 1 "party1")
                        (Party "party2")
                        (Constant 500)
                        (Pay
                            (AccountId 2 "party2")
                            (Party "party1")
                            (Constant 300)
                            Refund
                        )
                    )
                ]
                20
                Refund
            )
        , Case
            (Deposit
                (AccountId 2 "party2")
                "party2"
                (Constant 300)
            )
            (When
                [ Case
                    (Deposit
                        (AccountId 1 "party1")
                        "party1"
                        (Constant 500)
                    )
                    (Pay
                        (AccountId 1 "party1")
                        (Account
                            (AccountId 2 "party2")
                        )
                        (Constant 200)
                        Refund
                    )
                ]
                20
                Refund
            )
        ]
        15
        Refund


zeroCouponBond : Contract
zeroCouponBond =
    When
        [ Case
            (Deposit
                (AccountId 0 "investor")
                "investor"
                (Constant 850)
            )
            (Pay
                (AccountId 0 "investor")
                (Party "issuer")
                (Constant 850)
                (When
                    [ Case
                        (Deposit
                            (AccountId 0 "investor")
                            "issuer"
                            (Constant 1000)
                        )
                        (Pay
                            (AccountId 0 "investor")
                            (Party "investor")
                            (Constant 1000)
                            Refund
                        )
                    ]
                    20
                    Refund
                )
            )
        ]
        10
        Refund
