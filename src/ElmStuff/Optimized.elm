module ElmStuff.Optimized exposing
    ( Global(..), Expr(..), Position(..), Region(..), Def(..), Destructor(..), Path(..), Decider(..), Choice(..), Main(..), Node(..), EffectsType(..), Chunk(..), Test(..), DecisionTreePath(..), Artifacts, GlobalGraph, LocalGraph
    , binArtifacts, binGlobal, binExpr, binPosition, binRegion, binDef, binDestructor, binPath, binDecider, binChoice, binGlobalGraph, binLocalGraph, binMain, binNode, binEffectsType, binChunk, binTest, binDecisionTreePath
    )

{-|


## Types

@docs Global, Expr, Position, Region, Def, Destructor, Path, Decider, Choice, Main, Node, EffectsType, Chunk, Test, DecisionTreePath, Artifacts, GlobalGraph, LocalGraph


## Decoders

@docs binArtifacts, binGlobal, binExpr, binPosition, binRegion, binDef, binDestructor, binPath, binDecider, binChoice, binGlobalGraph, binLocalGraph, binMain, binNode, binEffectsType, binChunk, binTest, binDecisionTreePath

-}

import Bytes.Decode exposing (..)
import Dict exposing (Dict)
import DictAny
import ElmStuff.Canonical as Canonical
import ElmStuff.Interface as Interface
import HelpBytes exposing (..)


{-| -}
type alias Artifacts =
    { ifaces : Dict Name Interface.DependencyInterface
    , objects : GlobalGraph
    }


{-| -}
binArtifacts : Decoder Artifacts
binArtifacts =
    map2 Artifacts (dict binName Interface.binDependencyInterface) binGlobalGraph


{-| -}
type Global
    = Global Canonical.PackageCanonical Name


keysGlobal : Global -> String
keysGlobal (Global pack na) =
    [ pack.name
    , pack.package.author
    , pack.package.project
    , na
    ]
        |> String.join "_"


{-| -}
binGlobal : Decoder Global
binGlobal =
    map2 Global Canonical.binPackageCanonical binName


{-| -}
type Expr
    = ExprBool Bool
    | ExprChr String
    | ExprStr String
    | ExprInt Int
    | ExprFloat Float
    | ExprVarLocal Name
    | ExprVarGlobal Global
    | ExprVarEnum Global ZeroBased
    | ExprVarBox Global
    | ExprVarCycle Canonical.PackageCanonical Name
    | ExprVarDebug Name Canonical.PackageCanonical Region (Maybe Name)
    | ExprVarKernel Name Name
    | ExprList (List Expr)
    | ExprFunction (List Name) Expr
    | ExprCall Expr (List Expr)
    | ExprTailCall Name (List ( Name, Expr ))
    | ExprIf (List ( Expr, Expr )) Expr
    | ExprLet Def Expr
    | ExprDestruct Destructor Expr
    | ExprCase Name Name Decider (List ( Int, Expr ))
    | ExprAccessor Name
    | ExprAccess Expr Name
    | ExprUpdate Expr (Dict Name Expr)
    | ExprRecord (Dict Name Expr)
    | ExprUnit
    | ExprTuple Expr Expr (Maybe Expr)
    | ExprShader String (List Name) (List Name)


{-| -}
binExpr : Decoder Expr
binExpr =
    oneOf <|
        \n ->
            case n of
                0 ->
                    map ExprBool binBool

                1 ->
                    map ExprChr binString

                -- check if binChar is needed
                2 ->
                    map ExprStr binString

                3 ->
                    map ExprInt binInt

                4 ->
                    map ExprFloat binFloat

                5 ->
                    map ExprVarLocal binName

                6 ->
                    map ExprVarGlobal binGlobal

                7 ->
                    map2 ExprVarEnum binGlobal binZeroBased

                8 ->
                    map ExprVarBox binGlobal

                9 ->
                    map2 ExprVarCycle Canonical.binPackageCanonical binName

                10 ->
                    map4 ExprVarDebug binName Canonical.binPackageCanonical binRegion (maybe binName)

                11 ->
                    map2 ExprVarKernel binName binName

                12 ->
                    map ExprList (list binExpr)

                13 ->
                    map2 ExprFunction (list binName) binExpr

                14 ->
                    map2 ExprCall binExpr (list binExpr)

                15 ->
                    map2 ExprTailCall binName (list (pair binName binExpr))

                16 ->
                    map2 ExprIf (list (pair binExpr binExpr)) binExpr

                17 ->
                    map2 ExprLet binDef binExpr

                18 ->
                    map2 ExprDestruct binDestructor binExpr

                19 ->
                    map4 ExprCase binName binName binDecider (list (pair binInt binExpr))

                20 ->
                    map ExprAccessor binName

                21 ->
                    map2 ExprAccess binExpr binName

                22 ->
                    map2 ExprUpdate binExpr (dict binName binExpr)

                23 ->
                    map ExprRecord (dict binName binExpr)

                24 ->
                    succeed ExprUnit

                25 ->
                    map3 ExprTuple binExpr binExpr (maybe binExpr)

                26 ->
                    map3 ExprShader binString (list binName) (list binName)

                _ ->
                    fail


{-| -}
type Position
    = Position Word16 Word16


{-| -}
binPosition : Decoder Position
binPosition =
    map2 Position word16 word16


{-| -}
type Region
    = Region Position Position


{-| -}
binRegion : Decoder Region
binRegion =
    map2 Region binPosition binPosition


{-| -}
type Def
    = Def Name Expr
    | TailDef Name (List Name) Expr


{-| -}
binDef : Decoder Def
binDef =
    oneOf <|
        \n ->
            case n of
                0 ->
                    map2 Def binName binExpr

                1 ->
                    map3 TailDef binName (list binName) binExpr

                _ ->
                    fail


{-| -}
type Destructor
    = Destructor Name Path


{-| -}
binDestructor : Decoder Destructor
binDestructor =
    map2 Destructor binName binPath


{-| -}
type Path
    = Index ZeroBased Path
    | Field Name Path
    | Unbox Path
    | Root Name


{-| -}
binPath : Decoder Path
binPath =
    oneOf <|
        \n ->
            case n of
                0 ->
                    map2 Index binZeroBased binPath

                1 ->
                    map2 Field binName binPath

                2 ->
                    map Unbox binPath

                3 ->
                    map Root binName

                _ ->
                    fail



-- BRANCHING


{-| -}
type Decider
    = Leaf Choice
    | Chain
        { testChain : List ( DecisionTreePath, Test )
        , success : Decider
        , failure : Decider
        }
    | FanOut
        { path : DecisionTreePath
        , tests : List ( Test, Decider )
        , fallback : Decider
        }


deciderChain : List ( DecisionTreePath, Test ) -> Decider -> Decider -> Decider
deciderChain testChain success failure =
    Chain
        { testChain = testChain
        , success = success
        , failure = failure
        }


deciderFanOut : DecisionTreePath -> List ( Test, Decider ) -> Decider -> Decider
deciderFanOut path tests fallback =
    FanOut
        { path = path
        , tests = tests
        , fallback = fallback
        }


{-| -}
binDecider : Decoder Decider
binDecider =
    oneOf <|
        \n ->
            case n of
                0 ->
                    map Leaf binChoice

                1 ->
                    map3 deciderChain (list (pair binDecisionTreePath binTest)) binDecider binDecider

                2 ->
                    map3 deciderFanOut binDecisionTreePath (list (pair binTest binDecider)) binDecider

                _ ->
                    fail


{-| -}
type Choice
    = Inline Expr
    | Jump Int


{-| -}
binChoice : Decoder Choice
binChoice =
    oneOf <|
        \n ->
            case n of
                0 ->
                    map Inline binExpr

                1 ->
                    map Jump binInt

                _ ->
                    fail



-- OBJECT GRAPH


{-| -}
type alias GlobalGraph =
    { g_nodes : DictAny.Dict Global Node
    , g_fields : Dict Name Int
    }


{-| -}
binGlobalGraph : Decoder GlobalGraph
binGlobalGraph =
    map2 GlobalGraph (dictAny keysGlobal binGlobal binNode) (dict binName binInt)


{-| -}
type alias LocalGraph =
    { l_main : Maybe Main
    , l_nodes : DictAny.Dict Global Node -- PERF profile switching Global to Name
    , l_fields : Dict Name Int
    }


{-| -}
binLocalGraph : Decoder LocalGraph
binLocalGraph =
    map3 LocalGraph (maybe binMain) (dictAny keysGlobal binGlobal binNode) (dict binName binInt)


{-| -}
type Main
    = Static
    | Dynamic
        { message : Canonical.Type
        , decoder : Expr
        }


{-| -}
binMain : Decoder Main
binMain =
    oneOf <|
        \n ->
            case n of
                0 ->
                    succeed Static

                1 ->
                    map2 mainDynamic Canonical.binType binExpr

                _ ->
                    fail


mainDynamic : Canonical.Type -> Expr -> Main
mainDynamic message decoder =
    Dynamic
        { message = message
        , decoder = decoder
        }


{-| -}
type Node
    = Define Expr (List Global)
    | DefineTailFunc (List Name) Expr (List Global)
    | Ctor ZeroBased Int
    | Enum ZeroBased
    | Box
    | Link Global
    | Cycle (List Name) (List ( Name, Expr )) (List Def) (List Global)
    | Manager EffectsType
    | Kernel (List Chunk) (List Global)
    | PortIncoming Expr (List Global)
    | PortOutgoing Expr (List Global)


{-| -}
binNode : Decoder Node
binNode =
    oneOf <|
        \n ->
            case n of
                0 ->
                    map2 Define binExpr (list binGlobal)

                1 ->
                    map3 DefineTailFunc (list binName) binExpr (list binGlobal)

                2 ->
                    map2 Ctor binZeroBased binInt

                3 ->
                    map Enum binZeroBased

                4 ->
                    succeed Box

                5 ->
                    map Link binGlobal

                6 ->
                    map4 Cycle (list binName) (list (pair binName binExpr)) (list binDef) (list binGlobal)

                7 ->
                    map Manager binEffectsType

                8 ->
                    map2 Kernel (list binChunk) (list binGlobal)

                9 ->
                    map2 PortIncoming binExpr (list binGlobal)

                10 ->
                    map2 PortOutgoing binExpr (list binGlobal)

                _ ->
                    fail


{-| -}
type EffectsType
    = Cmd
    | Sub
    | Fx


{-| -}
binEffectsType : Decoder EffectsType
binEffectsType =
    oneOf <|
        \n ->
            case n of
                0 ->
                    succeed Cmd

                1 ->
                    succeed Sub

                2 ->
                    succeed Fx

                _ ->
                    fail


{-| -}
type Chunk
    = JS String
    | ElmVar Canonical.Canonical Name
    | JsVar Name Name
    | ElmField Name
    | JsField Int
    | JsEnum Int
    | Debug_
    | Prod


{-| -}
binChunk : Decoder Chunk
binChunk =
    oneOf <|
        \n ->
            case n of
                0 ->
                    map JS binString

                1 ->
                    map2 ElmVar Canonical.binCanonical binName

                2 ->
                    map2 JsVar binName binName

                3 ->
                    map ElmField binName

                4 ->
                    map JsField binInt

                5 ->
                    map JsEnum binInt

                6 ->
                    succeed Debug_

                7 ->
                    succeed Prod

                _ ->
                    fail


{-| -}
type Test
    = IsCtor Canonical.Canonical Name ZeroBased Int Canonical.CtorOpts
    | IsCons
    | IsNil
    | IsTuple
    | IsInt Int
    | IsChr String
    | IsStr String
    | IsBool Bool


{-| -}
binTest : Decoder Test
binTest =
    oneOf <|
        \n ->
            case n of
                0 ->
                    map5 IsCtor Canonical.binCanonical binName binZeroBased binInt Canonical.binCtorOpts

                1 ->
                    succeed IsCons

                2 ->
                    succeed IsNil

                3 ->
                    succeed IsTuple

                4 ->
                    map IsChr binString

                5 ->
                    map IsStr binString

                6 ->
                    map IsInt binInt

                7 ->
                    map IsBool binBool

                _ ->
                    fail


{-| -}
type DecisionTreePath
    = DTIndex ZeroBased DecisionTreePath
    | DTUnbox DecisionTreePath
    | DTEmpty


{-| -}
binDecisionTreePath : Decoder DecisionTreePath
binDecisionTreePath =
    oneOf <|
        \n ->
            case n of
                0 ->
                    map2 DTIndex binZeroBased binDecisionTreePath

                1 ->
                    map DTUnbox binDecisionTreePath

                2 ->
                    succeed DTEmpty

                _ ->
                    fail
