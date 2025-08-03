module ComplexModule exposing (..)

-- Custom types (discriminated unions)
type Color
    = Red
    | Green
    | Blue
    | RGB Int Int Int

type Maybe a
    = Just a
    | Nothing

type Result error value
    = Ok value
    | Err error

type Tree a
    = Leaf
    | Node a (Tree a) (Tree a)

-- Type aliases
type alias User =
    { id : Int
    , name : String
    , email : String
    , isActive : Bool
    }

type alias Point =
    { x : Float
    , y : Float
    }

type alias Config =
    { debug : Bool
    , apiUrl : String
    , timeout : Int
    , endpoints : List String
    }

-- Functions with various signatures
identity : a -> a
identity x = x

map : (a -> b) -> List a -> List b
map f list =
    case list of
        [] -> []
        x :: xs -> f x :: map f xs

fold : (a -> b -> b) -> b -> List a -> b
fold func acc list =
    case list of
        [] -> acc
        x :: xs -> fold func (func x acc) xs

-- Functions returning tuples
getCoordinates : Point -> (Float, Float)
getCoordinates point = (point.x, point.y)

split : List a -> (List a, List a)
split list =
    let
        len = List.length list
        half = len // 2
    in
    (List.take half list, List.drop half list)

-- Functions with records
createUser : String -> String -> User
createUser name email =
    { id = 0
    , name = name
    , email = email
    , isActive = True
    }

updateUserEmail : String -> User -> User
updateUserEmail newEmail user =
    { user | email = newEmail }

-- Functions with custom types
colorToString : Color -> String
colorToString color =
    case color of
        Red -> "red"
        Green -> "green"
        Blue -> "blue"
        RGB r g b -> "rgb(" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ ")"

parseColor : String -> Maybe Color
parseColor str =
    case str of
        "red" -> Just Red
        "green" -> Just Green
        "blue" -> Just Blue
        _ -> Nothing

-- Complex nested types
type alias TodoItem =
    { id : Int
    , title : String
    , completed : Bool
    , tags : List String
    }

type alias TodoList =
    { name : String
    , items : List TodoItem
    , owner : User
    }

-- Functions with nested records
createTodoList : String -> User -> TodoList
createTodoList name owner =
    { name = name
    , items = []
    , owner = owner
    }

addTodo : String -> List String -> TodoList -> TodoList
addTodo title tags todoList =
    let
        newItem =
            { id = List.length todoList.items
            , title = title
            , completed = False
            , tags = tags
            }
    in
    { todoList | items = todoList.items ++ [ newItem ] }

-- Higher order functions
compose : (b -> c) -> (a -> b) -> (a -> c)
compose g f x = g (f x)

flip : (a -> b -> c) -> (b -> a -> c)
flip f x y = f y x

curry : ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry : (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

-- Functions with multiple type parameters
zip : List a -> List b -> List (a, b)
zip listA listB =
    case (listA, listB) of
        ([], _) -> []
        (_, []) -> []
        (a :: restA, b :: restB) -> (a, b) :: zip restA restB

-- Nested custom types
type Status
    = Active User
    | Inactive { reason : String, since : String }
    | Pending (Maybe String)

getStatusMessage : Status -> String
getStatusMessage status =
    case status of
        Active user -> user.name ++ " is active"
        Inactive record -> "Inactive since " ++ record.since ++ ": " ++ record.reason
        Pending maybeReason ->
            case maybeReason of
                Just reason -> "Pending: " ++ reason
                Nothing -> "Pending"