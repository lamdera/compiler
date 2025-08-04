module JsonExample exposing (parseUser, encodeUser, User)

import Json.Decode as Decode
import Json.Encode as Encode

type alias User =
    { id : Int
    , name : String
    , email : String
    }

parseUser : String -> Result String User  
parseUser json =
    case Decode.decodeString userDecoder json of
        Ok user -> Ok user
        Err error -> Err (Decode.errorToString error)

userDecoder : Decode.Decoder User
userDecoder =
    Decode.map3 User
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "email" Decode.string)

encodeUser : User -> String
encodeUser user =
    Encode.encode 0 (userEncoder user)

userEncoder : User -> Encode.Value
userEncoder user =
    Encode.object
        [ ("id", Encode.int user.id)
        , ("name", Encode.string user.name)
        , ("email", Encode.string user.email)
        ]