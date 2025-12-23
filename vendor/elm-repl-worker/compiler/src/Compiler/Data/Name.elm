module Compiler.Data.Name exposing
    ( Name
    , array
    , bName
    , basics
    , bitwise
    , bool
    , char
    , cmd
    , debug
    , debugger
    , dict
    , dollar
    , false
    , float
    , fromManyNames
    , fromPtr
    , fromTypeVariable
    , fromTypeVariableScheme
    , fromVarIndex
    , fromWords
    , getKernel
    , hasDot
    , identity_
    , int
    , isAppendableType
    , isCompappendType
    , isComparableType
    , isKernel
    , isNumberType
    , jsArray
    , l_main
    , list
    , maybe
    , negate
    , node
    , platform
    , program
    , replModule
    , replValueToPrint
    , result
    , router
    , sepBy
    , shader
    , splitDots
    , string
    , sub
    , task
    , toBuilder
    , toElmString
    , true
    , tuple
    , u_Main
    , utils
    , value
    , virtualDom
    )

import Compiler.Data.Utf8 as Utf8
import Compiler.Elm.String as ES
import Extra.Data.Binary as B
import Extra.Type.List as MList exposing (TList)



-- NAME


type alias Name =
    Utf8.Utf8



-- TO


toElmString : Name -> ES.TString
toElmString =
    identity


toBuilder : Name -> String
toBuilder =
    Utf8.toBuilder



-- FROM


fromPtr : String -> Int -> Int -> Name
fromPtr =
    Utf8.fromPtr



-- HAS DOT


hasDot : Name -> Bool
hasDot name =
    Utf8.contains 0x2E {- . -} name


splitDots : Name -> TList Name
splitDots name =
    Utf8.split 0x2E {- . -} name



-- GET KERNEL


getKernel : Name -> Name
getKernel name =
    if isKernel name then
        String.dropLeft 11 name

    else
        Debug.todo "getKernel: not a kernel name"



-- STARTS WITH


isKernel : Name -> Bool
isKernel =
    Utf8.startsWith prefix_kernel


isNumberType : Name -> Bool
isNumberType =
    Utf8.startsWith prefix_number


isComparableType : Name -> Bool
isComparableType =
    Utf8.startsWith prefix_comparable


isAppendableType : Name -> Bool
isAppendableType =
    Utf8.startsWith prefix_appendable


isCompappendType : Name -> Bool
isCompappendType =
    Utf8.startsWith prefix_compappend


prefix_kernel : Name
prefix_kernel =
    "Elm.Kernel."


prefix_number : Name
prefix_number =
    "number"


prefix_comparable : Name
prefix_comparable =
    "comparable"


prefix_appendable : Name
prefix_appendable =
    "appendable"


prefix_compappend : Name
prefix_compappend =
    "compappend"



-- FROM VAR INDEX


fromVarIndex : Int -> Name
fromVarIndex n =
    writeDigitsAtEnd n "_v"


writeDigitsAtEnd : Int -> String -> String
writeDigitsAtEnd n s =
    s ++ String.fromInt n



-- FROM TYPE VARIABLE


fromTypeVariable : Name -> Int -> Name
fromTypeVariable name index =
    if index <= 0 then
        name

    else
        let
            end =
                String.right 1 name
        in
        if "0" <= end && end <= "9" then
            name ++ "_" ++ String.fromInt index

        else
            name ++ String.fromInt index



-- FROM TYPE VARIABLE SCHEME


fromTypeVariableScheme : Int -> Name
fromTypeVariableScheme scheme =
    if scheme < 26 then
        String.fromChar (Char.fromCode (0x61 + scheme))

    else
        let
            extra =
                scheme // 26

            letter =
                modBy 26 scheme
        in
        writeDigitsAtEnd extra <| String.fromChar (Char.fromCode (0x61 + letter))



-- FROM MANY NAMES
--
-- Creating a unique name by combining all the subnames can create names
-- longer than 256 bytes relatively easily. So instead, the first given name
-- (e.g. foo) is prefixed chars that are valid in JS but not Elm (e.g. _M$foo)
--
-- This should be a unique name since 0.19 disallows shadowing. It would not
-- be possible for multiple top-level cycles to include values with the same
-- name, so the important thing is to make the cycle name distinct from the
-- normal name. Same logic for destructuring patterns like (x,y)


fromManyNames : TList Name -> Name
fromManyNames names =
    case names of
        [] ->
            blank

        -- NOTE: this case is needed for (let _ = Debug.log "x" x in ...)
        -- but maybe unused patterns should be stripped out instead
        name :: _ ->
            "_M$" ++ name


blank : Name
blank =
    "_M$"



-- FROM WORDS


fromWords : TList Int -> Name
fromWords words =
    words
        |> MList.map Char.fromCode
        |> String.fromList



-- SEP BY


sepBy : Int -> Name -> Name -> Name
sepBy sep name1 name2 =
    name1 ++ String.cons (Char.fromCode sep) name2



-- COMMON NAMES


int : Name
int =
    "Int"


float : Name
float =
    "Float"


bool : Name
bool =
    "Bool"


char : Name
char =
    "Char"


string : Name
string =
    "String"


maybe : Name
maybe =
    "Maybe"


result : Name
result =
    "Result"


list : Name
list =
    "List"


array : Name
array =
    "Array"


dict : Name
dict =
    "Dict"


tuple : Name
tuple =
    "Tuple"


jsArray : Name
jsArray =
    "JsArray"


task : Name
task =
    "Task"


router : Name
router =
    "Router"


cmd : Name
cmd =
    "Cmd"


sub : Name
sub =
    "Sub"


platform : Name
platform =
    "Platform"


virtualDom : Name
virtualDom =
    "VirtualDom"


shader : Name
shader =
    "Shader"


debug : Name
debug =
    "Debug"


debugger : Name
debugger =
    "Debugger"


bitwise : Name
bitwise =
    "Bitwise"


basics : Name
basics =
    "Basics"


utils : Name
utils =
    "Utils"


negate : Name
negate =
    "negate"


true : Name
true =
    "True"


false : Name
false =
    "False"


value : Name
value =
    "Value"


node : Name
node =
    "Node"


program : Name
program =
    "Program"


l_main : Name
l_main =
    "main"


u_Main : Name
u_Main =
    "Main"


dollar : Name
dollar =
    "$"


identity_ : Name
identity_ =
    "identity"


replModule : Name
replModule =
    "Elm_Repl"


replValueToPrint : Name
replValueToPrint =
    "repl_input_value_"



-- BINARY


bName : B.Binary Name
bName =
    Utf8.bUnder256
