{- MANUALLY FORMATTED -}
module Terminal.Main exposing
  ( runMain
  )


import Compiler.Elm.Version as V
import Compiler.Reporting.Doc as D exposing (d, da)
import Extra.System.IO as IO
import Extra.Type.List as MList
import Terminal.Command as Command
import Terminal.Terminal as Terminal



-- PRIVATE IO


type alias IO g h v =
  IO.IO (Command.GlobalState g h) v



-- RUN


runMain : IO g h ()
runMain =
  Terminal.app intro outro
    [ init
    , install
    , make
    , bump
    , diff
    , publish
    , reactor
    , repl
    ]


intro : D.Doc
intro =
  D.vcat
    [ D.fillSep
        [d"Hi,",d"thank",d"you",d"for",d"trying",d"out"
        ,D.greenS "Elm"
        ,da[D.greenS (V.toChars V.compiler), d"."]
        ,d"I hope you like it!"
        ]
    , d""
    , D.blackS "-------------------------------------------------------------------------------"
    , D.blackS "I highly recommend working through <https://guide.elm-lang.org> to get started."
    , D.blackS "It teaches many important concepts, including how to use `elm` in the terminal."
    , D.blackS "-------------------------------------------------------------------------------"
    ]


outro : D.Doc
outro =
  D.fillSep <| MList.map D.fromChars <| String.words <|
    "Be sure to ask on the Elm slack if you run into trouble! Folks are friendly and"
    ++ " happy to help out. They hang out there because it is fun, so be kind to get the"
    ++ " best results!"



-- INIT


init : Terminal.Command
init =
  let
    summary =
      "Start an Elm project. It creates a starter elm.json file and"
      ++ " provides a link explaining what to do from there."
  in
  Terminal.command "init" (Terminal.common summary)



-- REPL


repl : Terminal.Command
repl =
  let
    summary =
      "Open up an interactive programming session. Type in Elm expressions"
      ++ " like (2 + 2) or (String.length \"test\") and see if they equal four!"
  in
  Terminal.command "repl" (Terminal.common summary)



-- REACTOR


reactor : Terminal.Command
reactor =
  let
    summary =
      "Compile code with a click. It opens a file viewer in your browser, and"
      ++ " when you click on an Elm file, it compiles and you see the result."
  in
  Terminal.command "reactor" (Terminal.common summary)



-- MAKE


make : Terminal.Command
make =
  Terminal.command "make" Terminal.uncommon



-- INSTALL


install : Terminal.Command
install =
  Terminal.command "install" Terminal.uncommon



-- PUBLISH


publish : Terminal.Command
publish =
  Terminal.command "publish" Terminal.uncommon



-- BUMP


bump : Terminal.Command
bump =
  Terminal.command "bump" Terminal.uncommon



-- DIFF


diff : Terminal.Command
diff =
  Terminal.command "diff" Terminal.uncommon
