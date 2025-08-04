module TestModule exposing (add, greet, processUser, User)

type alias User =
    { name : String
    , age : Int
    }

add : Int -> Int -> Int
add x y = x + y

greet : String -> String -> String
greet firstName lastName = 
    "Hello, " ++ firstName ++ " " ++ lastName ++ "!"

processUser : User -> String
processUser user =
    user.name ++ " is " ++ String.fromInt user.age ++ " years old"