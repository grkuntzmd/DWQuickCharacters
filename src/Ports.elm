port module Ports
    exposing
        ( deleteItem
        , getItem
        , getNames
        , loadItem
        , loadNames
        , saveItem
        , showDialog
        )


port deleteItem : String -> Cmd msg


port getItem : (String -> msg) -> Sub msg


port getNames : (List ( String, String ) -> msg) -> Sub msg


port loadItem : String -> Cmd msg


port loadNames : () -> Cmd msg


port saveItem : ( String, String ) -> Cmd msg


port showDialog : String -> Cmd msg
