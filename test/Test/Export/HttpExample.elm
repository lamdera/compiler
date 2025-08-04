module HttpExample exposing (fetchUser, parseUser, User)

import Http
import Json.Decode as Decode

type alias User =
    { id : Int
    , name : String
    , email : String
    }

userDecoder : Decode.Decoder User
userDecoder =
    Decode.map3 User
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "email" Decode.string)

fetchUser : Int -> Cmd Msg
fetchUser userId =
    Http.get
        { url = "https://api.example.com/users/" ++ String.fromInt userId
        , expect = Http.expectJson GotUser userDecoder
        }

parseUser : String -> Result String User
parseUser json =
    case Decode.decodeString userDecoder json of
        Ok user -> Ok user
        Err error -> Err (Decode.errorToString error)

type Msg = GotUser (Result Http.Error User)