module ElmStuff.Interface exposing
    ( Union(..), Alias(..), DependencyInterface(..), Associativity(..), Interfaces, Interface, Binop, Precedence
    , binInterfaces, binInterface, binUnion, binAlias, binBinop, binDependencyInterface, binAssociativity
    )

{-|


## Types

@docs Union, Alias, DependencyInterface, Associativity, Interfaces, Interface, Binop, Precedence


## Decoders

@docs binInterfaces, binInterface, binUnion, binAlias, binBinop, binDependencyInterface, binAssociativity

-}

import Bytes.Decode exposing (..)
import Dict exposing (Dict)
import DictAny
import ElmStuff.Canonical as Canonical
import HelpBytes exposing (..)


{-| -}
type alias Interfaces =
    DictAny.Dict Canonical.Canonical DependencyInterface


{-| -}
binInterfaces : Decoder Interfaces
binInterfaces =
    dictAny
        (\a ->
            [ a.package.author
            , a.package.project
            , a.module_
            ]
                |> String.join "_"
        )
        Canonical.binCanonical
        binDependencyInterface


{-| -}
type alias Interface =
    { home : Canonical.PackageName
    , values : Dict Name Canonical.Annotation
    , unions : Dict Name Union
    , aliases : Dict Name Alias
    , binops : Dict Name Binop
    }


{-| -}
binInterface : Decoder Interface
binInterface =
    map5 Interface
        Canonical.binPackageName
        (dict binName Canonical.binAnnotation)
        (dict binName binUnion)
        (dict binName binAlias)
        (dict binName binBinop)


{-| -}
type Union
    = OpenUnion Canonical.Union
    | ClosedUnion Canonical.Union
    | PrivateUnion Canonical.Union


{-| -}
binUnion : Decoder Union
binUnion =
    oneOf <|
        \n ->
            case n of
                0 ->
                    map OpenUnion Canonical.binUnion

                1 ->
                    map ClosedUnion Canonical.binUnion

                2 ->
                    map PrivateUnion Canonical.binUnion

                _ ->
                    fail


{-| -}
type Alias
    = PublicAlias Canonical.Alias
    | PrivateAlias Canonical.Alias


{-| -}
binAlias : Decoder Alias
binAlias =
    oneOf <|
        \n ->
            case n of
                0 ->
                    map PublicAlias Canonical.binAlias

                1 ->
                    map PrivateAlias Canonical.binAlias

                _ ->
                    fail


{-| -}
type alias Binop =
    { op_name : Name
    , op_annotation : Canonical.Annotation
    , op_associativity : Associativity
    , op_precedence : Precedence
    }


{-| -}
binBinop : Decoder Binop
binBinop =
    map4 Binop binName Canonical.binAnnotation binAssociativity binInt


{-| -}
type DependencyInterface
    = Public Interface
    | Private Canonical.PackageName (Dict Name Canonical.Union) (Dict Name Canonical.Alias)


{-| -}
binDependencyInterface : Decoder DependencyInterface
binDependencyInterface =
    oneOf <|
        \n ->
            case n of
                0 ->
                    map Public binInterface

                1 ->
                    map3 Private Canonical.binPackageName (dict binName Canonical.binUnion) (dict binName Canonical.binAlias)

                _ ->
                    fail


{-| -}
type alias Precedence =
    Int


{-| -}
type Associativity
    = Left
    | Non
    | Right


{-| -}
binAssociativity : Decoder Associativity
binAssociativity =
    oneOf <|
        \n ->
            case n of
                0 ->
                    succeed Left

                1 ->
                    succeed Non

                2 ->
                    succeed Right

                _ ->
                    fail
