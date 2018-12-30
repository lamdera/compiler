Copied from https://gist.github.com/matheus23/fdc7f3c78b3413b551a1dac8a46375be


# The Elm Compiler Architecture

## Common Datastructures

* `Elm.Compiler.Module.Raw` is just an alias for `Elm.Name.Name`, which is
    a newtype wrapper around `Data.Text.Text`.
    Conceptually, a `Module.Raw` is ??? (is it canonicalized, or not?)

## Understanding the Compiler Pipeline

Compiling a List of `FilePath`s can be done as follows:

```haskell
compileSome :: FilePath -> [FilePath]
compileSome elmJsonFile files = do
    Reporting.Task.try Reporting.Progress.Json.reporter $ do
        project <- Elm.Project.Json.read elmJsonFile
        Elm.Project.Json.check project
        summary <- Stuff.Verify.verify root project
        args <- File.Args.fromPaths summary files
        graph <- File.Crawl.crawl summary args
        (dirty, ifaces) <- File.Plan.plan Nothing summary graph
        File.Compile.compile project Nothing ifaces dirty
```

Dissecting the above function:

### Progress Reporters and Tasks

A `Reporter` is the interface the elm compiler uses to talk to the outside
world (except for file IO).

The datatype is defined in `Reporting.Progress`:
```haskell
data Reporter =
  Reporter
    { _tell :: Progress -> IO ()
    , _ask :: D.Doc -> IO Bool
    , _end :: Maybe Exit -> IO ()
    }
```

* `_tell` is used to output something. That `Progress` is a union type of all
    different messages to output to the user, something along the lines of
    "started/finished compiling module X", "started/finished downloading Y".
    The `Progress` datatype is defined in `Reporting.Progress`.
* `_ask` is used to prompt the user for a yes/no question (for example to ask
    for confirmation before downloading and installing a package).
* `_end` is used signal that the compiler should be terminated.

A big part of the compiler pipeline is defined in the `Task` monad, which
provides read access to a `Reporter`.

The goal of the reporter abstraction is to make the different operation modes
of the elm compiler possible:
* Human interaction mode, when running the compiler
    from the command line as ususal: `elm make ...`
* Json mode, when providing the `--report=json` flag, which can be used to
    implement elm editor integrations (by making its output machine-readable)

### The elm.json File

The `elm.json` file is parsed into a `Elm.Project.Summary.Summary`. This
summary also holds a `Elm.Interface.Interfaces` (which is a really useful
datastructure for code completion), however, it doesn't get populated with the
latest state by `Stuff.Verify.verify`. It just adds all elm interfaces (`.elmi`
files) from the elm-stuff cache (honestly this is just an assumption from
skipping through the source code. I haven't checked this.).

### The Args datastructure

Its basically the command line args from `elm make`. If you write an Elm package
(= library), then the args can be populated with your library surface modules.
If you write an application (~ your `elm.json` specifies an application), then
you have to specify entry points to `elm make`. Those would be stored as
`File.Args.Roots firstRoot [moreRoots, ...]` in the normal compiler execution.

### The crawler

`File.Crawl.crawl` builds a dependency graph of your whole Application/Package.
It uses the `Args` structure as starting points for computing dependencies
and builds a graph of what references/depends on what.

This graph will have the `File.Crawl.Result` type:

```haskell
data Graph kernel problems =
  Graph
    { _args :: Args.Args Module.Raw
    , _locals :: Map.Map Module.Raw Header.Info
    , _kernels :: Map.Map Module.Raw kernel
    , _foreigns :: Map.Map Module.Raw Pkg.Package
    , _problems :: problems
    }


type Result =
  Graph Obj.Kernel ()
```

* `_args` is a translation of our starting files as `FilePath`s to `Module.Raw`s
* `_locals` contains all module header infos from the current project
* `_kernels` contains all kernel modules (in this project?)
* `_foreings` gives a map of what modules are in which package
* `_problems` ?

Interesting might also be `File.Find.find(Elm)`, which looks for a given module name
and reports whether it found it locally, in a package dependency or not at all
(in which case it throws a report error).

### The build Plan

After building a dependency graph, the build plan decides an order in which to
process modules, such that the graph is traversed from its
[source to its sink](http://wiki.engageeducation.org.au/wp-content/uploads/2015/09/Network-Flow-1.png).

It also recognizes which `.elmi` files are outdated and which can be reused for
compilation.
This can be seen in the types: Wheras the left side of the `File.Plan.plan` result
is a `Map Module.Raw Info`, the right side is a `Elm.Interface.Interfaces`.

* `Elm.Interface.Interfaces` holds type information about a module's content
    ```haskell
    type Interfaces =
        Map.Map ModuleName.Canonical Interface

    data Interface
        = Interface
        { _types   :: Map.Map N.Name Can.Annotation
        , _unions  :: Map.Map N.Name Union
        , _aliases :: Map.Map N.Name Alias
        , _binops  :: Map.Map N.Name Binop
        }
    ```
* `Elm.Plan.Info` only holds information about dependent modules:
    ```haskell
    data Info
        = Info
        { _path :: FilePath
        , _time :: Time.UTCTime
        , _src :: BS.ByteString
        , _clean :: [Module.Raw]
        , _dirty :: [Module.Raw]
        , _foreign :: [Module.Canonical]  -- TODO is this needed?
        }
    ```

So the left side of the result tuple is basically a set of dirty module names,
tagged with information about them.

### Compilation

Finally comes the compilation. The most interesting part about it is defined
in `Compile.compile`:

```haskell
compile :: DocsFlag -> Pkg.Name -> ImportDict -> I.Interfaces -> BS.ByteString -> Result i Artifacts
compile flag pkg importDict interfaces source =
  do
      valid <- Result.mapError Error.Syntax $
        Parse.program pkg source

      canonical <- Result.mapError Error.Canonicalize $
        Canonicalize.canonicalize pkg importDict interfaces valid

      let localizer = L.fromModule valid -- TODO should this be strict for GC?

      annotations <-
        runTypeInference localizer canonical

      () <-
        exhaustivenessCheck canonical

      graph <- Result.mapError (Error.Main localizer) $
        Optimize.optimize annotations canonical

      documentation <-
        genarateDocs flag canonical

      Result.ok $
        Artifacts
          { _elmi = I.fromModule annotations canonical
          , _elmo = graph
          , _docs = documentation
          }
```
