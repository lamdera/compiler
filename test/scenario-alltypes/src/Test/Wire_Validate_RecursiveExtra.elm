module Test.Wire_Validate_RecursiveExtra exposing (..)

{-| Additional recursive / awkward configurations, each exercised with a
validator. All must compile with their validators wired in.

  - `Chain`: self-recursion through `Maybe`.
  - `Rose a`: parameterised self-recursion through `List`, validated with a
    signature that uses the type variable (`Rose a -> Result String ()`). This
    checks that the recursive call (which threads the type-variable codec) and
    the validator call coexist.
  - `Ping` / `Pong`: two mutually-recursive custom types, each with its own
    validator. Both generated decoders end up in one recursive binding group and
    each must resolve its respective validator.
-}


type Chain
    = Chain Int (Maybe Chain)


w3_validate_Chain : Chain -> Result String ()
w3_validate_Chain (Chain n _) =
    if n >= 0 then
        Ok ()

    else
        Err "Chain value must be non-negative"


type Rose a
    = Rose a (List (Rose a))


w3_validate_Rose : Rose a -> Result String ()
w3_validate_Rose _ =
    Ok ()


type Ping
    = Ping (Maybe Pong)


type Pong
    = Pong (Maybe Ping)


w3_validate_Ping : Ping -> Result String ()
w3_validate_Ping _ =
    Ok ()


w3_validate_Pong : Pong -> Result String ()
w3_validate_Pong _ =
    Ok ()
