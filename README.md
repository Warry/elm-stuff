# Elm Stuff

Decode interfaces (.elmi, i.dat) and objects (.elmo, o.dat) from elm-stuff/

```elm
fetchElmi : String -> Cmd (Result Error ElmStuff.ELMI)
fetchElmi moduleName =
    Http.get
        { url = "/elm-stuff/0.19.1/" ++ moduleName ++ ".elmi"
        , expect = Http.expectBytes identity ElmStuff.elmi
        }
```

