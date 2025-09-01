{- MANUALLY FORMATTED -}
module Compiler.Type.Occurs exposing
  ( occurs
  )


import Compiler.Type.Type as Type
import Compiler.Type.UnionFind as UF
import Extra.System.IO.Pure as IO
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map



-- IO


type alias IO t a =
  UF.IO Type.Descriptor t a



-- OCCURS


occurs : Type.Variable -> IO t Bool
occurs var =
  occursHelp [] var False


occursHelp : (TList Type.Variable) -> Type.Variable -> Bool -> IO t Bool
occursHelp seen var foundCycle =
  if MList.elem var seen then
    IO.return True

  else
    IO.bind (UF.get var) <| \(Type.Descriptor content _ _ _) ->
    case content of
      Type.FlexVar _ ->
        IO.return foundCycle

      Type.FlexSuper _ _ ->
        IO.return foundCycle

      Type.RigidVar _ ->
        IO.return foundCycle

      Type.RigidSuper _ _ ->
        IO.return foundCycle

      Type.Structure term ->
        let newSeen = var :: seen in
        case term of
          Type.App1 _ _ args ->
            IO.foldrMList (occursHelp newSeen) foundCycle args

          Type.Fun1 a b ->
            IO.andThen (occursHelp newSeen a) <|
              occursHelp newSeen b foundCycle

          Type.EmptyRecord1 ->
            IO.return foundCycle

          Type.Record1 fields ext ->
            IO.andThen (occursHelp newSeen ext) <|
              IO.foldrMList (occursHelp newSeen) foundCycle (Map.elems fields)

          Type.Unit1 ->
            IO.return foundCycle

          Type.Tuple1 a b maybeC ->
            case maybeC of
              Nothing ->
                IO.andThen (occursHelp newSeen a) <|
                  occursHelp newSeen b foundCycle

              Just c ->
                IO.andThen (occursHelp newSeen a) <|
                  IO.andThen (occursHelp newSeen b) <|
                    occursHelp newSeen c foundCycle

      Type.Alias _ _ args _ ->
        IO.foldrMList (occursHelp (var::seen)) foundCycle (MList.map Tuple.second args)

      Type.Error ->
        IO.return foundCycle
