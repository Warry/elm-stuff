module ElmStuff.Canonical exposing
    ( Alias(..), Canonical, Union, Ctor(..), CtorOpts(..), Annotation(..), FreeVars, Type(..), AliasType(..), FieldType(..), PackageName, PackageCanonical, Version
    , binAlias, binCanonical, binUnion, binCtor, binCtorOpts, binAnnotation, binFreeVars, binType, binAliasType, binFieldType, binPackageName, binPackageCanonical, binVersion
    )

{-|


## Types

@docs Alias, Canonical, Union, Ctor, CtorOpts, Annotation, FreeVars, Type, AliasType, FieldType, PackageName, PackageCanonical, Version


## Decoders

@docs binAlias, binCanonical, binUnion, binCtor, binCtorOpts, binAnnotation, binFreeVars, binType, binAliasType, binFieldType, binPackageName, binPackageCanonical, binVersion

-}

import Bytes.Decode exposing (..)
import Dict exposing (Dict)
import HelpBytes exposing (..)


{-| Desc
-}
type Alias
    = Alias (List Name) Type


{-| -}
binAlias : Decoder Alias
binAlias =
    map2 Alias (list binName) binType


{-| -}
type alias Canonical =
    { package : PackageName
    , module_ : Name
    }


{-| -}
binCanonical : Decoder Canonical
binCanonical =
    map2 Canonical binPackageName binName


{-| -}
type alias Union =
    { u_vars : List Name
    , u_alts : List Ctor
    , u_numAlts : Int
    , u_opts : CtorOpts
    }


{-| -}
binUnion : Decoder Union
binUnion =
    map4 Union (list binName) (list binCtor) binInt binCtorOpts


{-| -}
type Ctor
    = Ctor Name ZeroBased Int (List Type) -- CACHE length args


{-| -}
binCtor : Decoder Ctor
binCtor =
    map4 Ctor binName binZeroBased binInt (list binType)


{-| -}
binCtorOpts : Decoder CtorOpts
binCtorOpts =
    oneOf <|
        \n ->
            case n of
                0 ->
                    succeed Normal

                1 ->
                    succeed Enum

                2 ->
                    succeed Unbox

                _ ->
                    fail


{-| -}
type CtorOpts
    = Normal
    | Enum
    | Unbox


{-| -}
binAnnotation : Decoder Annotation
binAnnotation =
    map2 Forall binFreeVars binType


{-| -}
type Annotation
    = Forall FreeVars Type


{-| -}
type alias FreeVars =
    Dict Name ()


{-| -}
binFreeVars : Decoder FreeVars
binFreeVars =
    dict binName (succeed ())


{-| -}
type Type
    = TLambda Type Type
    | TVar Name
    | TType Canonical Name (List Type)
    | TRecord (Dict Name FieldType) (Maybe Name)
    | TUnit
    | TTuple Type Type (Maybe Type)
    | TAlias Canonical Name (List ( Name, Type )) AliasType


{-| -}
type AliasType
    = Holey Type
    | Filled Type


{-| -}
type FieldType
    = FieldType Word16 Type


{-| -}
binType : Decoder Type
binType =
    oneOf binTypeStep


{-| -}
binTypeStep : Int -> Decoder Type
binTypeStep n =
    case n of
        0 ->
            map2 TLambda binType binType

        1 ->
            map TVar binName

        2 ->
            map2 TRecord (dict binName binFieldType) (maybe binName)

        3 ->
            succeed TUnit

        4 ->
            map3 TTuple binType binType (maybe binType)

        5 ->
            map4 TAlias binCanonical binName (list (pair binName binType)) binAliasType

        6 ->
            map3 TType binCanonical binName (map List.singleton binType)

        _ ->
            map3 TType binCanonical binName (loop ( n - 7, [] ) stepTType)


{-| -}
stepTType : ( Int, List Type ) -> Decoder (Step ( Int, List Type ) (List Type))
stepTType ( n, ls ) =
    if n == 0 then
        succeed (Done ls)

    else
        map (\x -> Loop ( n - 1, x :: ls )) binType


{-| -}
binAliasType : Decoder AliasType
binAliasType =
    oneOf <|
        \n ->
            case n of
                0 ->
                    map Holey binType

                1 ->
                    map Filled binType

                _ ->
                    fail


{-| -}
binFieldType : Decoder FieldType
binFieldType =
    map2 FieldType word16 binType


{-| -}
type alias PackageName =
    { author : Name
    , project : Name
    }


{-| -}
binPackageName : Decoder PackageName
binPackageName =
    map2 PackageName binName binName


{-| -}
type alias PackageCanonical =
    { name : Name
    , package : PackageName
    }


{-| -}
binPackageCanonical : Decoder PackageCanonical
binPackageCanonical =
    map2 PackageCanonical binName binPackageName


{-| -}
type alias Version =
    { major : Word16
    , minor : Word16
    , patch : Word16
    }


{-| -}
binVersion : Decoder Version
binVersion =
    map3 Version
        word16
        word16
        word16
