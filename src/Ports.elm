port module Ports exposing (getItem, getNames, loadItem, loadNames, saveItem, showDialog)


port getItem : (String -> msg) -> Sub msg


port getNames : (List ( String, String ) -> msg) -> Sub msg


port loadItem : String -> Cmd msg


port loadNames : () -> Cmd msg


port saveItem : ( String, String ) -> Cmd msg


port showDialog : String -> Cmd msg
