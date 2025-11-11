module ElmStuff exposing
    ( ELMI, elmi, IDAT, idat
    , ELMO, elmo, ODAT, odat
    )

{-|


## Interfaces

@docs ELMI, elmi, IDAT, idat


## Ojects

@docs ELMO, elmo, ODAT, odat

-}

import Bytes.Decode exposing (Decoder)
import ElmStuff.Interface as Interface
import ElmStuff.Optimized as Optimized


{-| Type representing .elmi files
-}
type alias ELMI =
    Interface.Interface


{-| Type representing i.dat files
-}
type alias IDAT =
    Interface.Interfaces


{-| Decode .elmi files from binaries

    Http.get
        { url = "/elm-stuff/0.19.1/" ++ moduleName ++ ".elmi"
        , expect = Http.expectBytes identity ElmStuff.elmi
        }

-}
elmi : Decoder ELMI
elmi =
    Interface.binInterface


{-| Decode i.dat files from binaries

    Http.get
        { url = "/elm-stuff/0.19.1/" ++ moduleName ++ ".idat"
        , expect = Http.expectBytes identity ElmStuff.idat
        }

-}
idat : Decoder IDAT
idat =
    Interface.binInterfaces


{-| Type representing .elmo files
-}
type alias ELMO =
    Optimized.LocalGraph


{-| Type representing o.dat files
-}
type alias ODAT =
    Optimized.GlobalGraph


{-| Decode .elmo files from binaries

    Http.get
        { url = "/elm-stuff/0.19.1/" ++ moduleName ++ ".elmo"
        , expect = Http.expectBytes identity ElmStuff.elmo
        }

-}
elmo : Decoder ELMO
elmo =
    Optimized.binLocalGraph


{-| Decode o.dat files from binaries

    Http.get
        { url = "/elm-stuff/0.19.1/" ++ moduleName ++ ".odat"
        , expect = Http.expectBytes identity ElmStuff.odat
        }

-}
odat : Decoder ODAT
odat =
    Optimized.binGlobalGraph
