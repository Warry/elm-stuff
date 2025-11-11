module HelpBytes exposing (..)

import Bytes exposing (Endianness(..))
import Bytes.Decode exposing (..)
import Dict exposing (Dict)
import DictAny


type alias ZeroBased =
    Int


type alias Name =
    String


type alias Word16 =
    String


oneOf : (Int -> Decoder b) -> Decoder b
oneOf fn =
    andThen fn unsignedInt8


list : Decoder a -> Decoder (List a)
list decoder =
    int64
        |> andThen (\len -> loop ( len, [] ) (listStep decoder))


listStep : Decoder a -> ( Int, List a ) -> Decoder (Step ( Int, List a ) (List a))
listStep decoder ( n, xs ) =
    if n <= 0 then
        succeed (Done xs)

    else
        map (\x -> Loop ( n - 1, x :: xs )) decoder


binZeroBased : Decoder ZeroBased
binZeroBased =
    int64


int64 : Decoder Int
int64 =
    loop ( 8, 0 ) int64Step


int64Step : ( Int, Int ) -> Decoder (Step ( Int, Int ) Int)
int64Step ( n, total ) =
    if n == 0 then
        succeed (Done total)

    else
        map
            (\x ->
                Loop ( n - 1, total + x * n ^ 8 )
            )
            unsignedInt8


drop : Int -> Decoder a -> Decoder a
drop n dec =
    bytes n
        |> andThen (\_ -> dec)


binName : Decoder Name
binName =
    unsignedInt8
        |> andThen string


word16 : Decoder Word16
word16 =
    string 2


dict : Decoder comparable -> Decoder b -> Decoder (Dict comparable b)
dict keys values =
    list (pair keys values)
        |> map Dict.fromList


maybe : Decoder a -> Decoder (Maybe a)
maybe dec =
    binBool
        |> andThen
            (\cond ->
                if cond then
                    map Just dec

                else
                    succeed Nothing
            )


binString : Decoder String
binString =
    int64
        |> andThen string


binBool : Decoder Bool
binBool =
    unsignedInt8
        |> andThen
            (\n ->
                case n of
                    0 ->
                        succeed False

                    1 ->
                        succeed True

                    _ ->
                        fail
            )


binInt : Decoder Int
binInt =
    int64


binFloat : Decoder Float
binFloat =
    binName
        |> andThen
            (\str ->
                case String.toFloat str of
                    Just f ->
                        succeed f

                    _ ->
                        fail
            )


pair : Decoder a -> Decoder b -> Decoder ( a, b )
pair =
    map2 Tuple.pair


dictAnyRaw : (a -> a -> Order) -> Decoder a -> Decoder b -> Decoder (DictAny.Dict a b)
dictAnyRaw compare keys values =
    list (pair keys values)
        |> map (DictAny.fromList compare)


dictAny : (a -> comparable) -> Decoder a -> Decoder b -> Decoder (DictAny.Dict a b)
dictAny compare =
    dictAnyRaw
        (\a b ->
            let
                keyA =
                    compare a

                keyB =
                    compare b
            in
            if keyA == keyB then
                EQ

            else if keyA > keyB then
                GT

            else
                LT
        )
