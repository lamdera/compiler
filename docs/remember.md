
### Injecting additional source-directories:


In /Users/mario/dev/projects/elmx/ui/terminal/src/Develop.hs

```
void $ Task.try reporter $
  do  summary <- Project.getRoot
      -- Inject here
      Project.compile Output.Dev Output.Client output Nothing summary [file]
```

```
let
  project = _project summary
  summaryMod =
    case project of
      App appInfo@(AppInfo _ srcDirs _ _ _ _) ->
        summary { _project = App (appInfo { _app_source_dirs = srcDirs ++ ["lamdera-stuff/localdev"] }) }
      Pkg info ->
        summary
```

Didn't need to use this in the end as Elm seems to happily compile a `.elm` outside of the source-directories.

So we could just compile `./lamdera-stuff/alpha/LocalDev.elm` directly.


There was also the lower level hack to do this for all projects:

/Users/mario/dev/projects/elmx/builder/src/Elm/Project/Json.hs

```
read :: FilePath -> Task.Task Project
read path =
  do  bytes <- liftIO $ BS.readFile path
      case D.parse "project" E.badContentToDocs decoder bytes of
        Left err ->
          throwBadJson (E.BadJson err)

        Right project@(Pkg _) ->
          return project

        Right project@(App (AppInfo _ srcDirs _ _ _ _)) ->
          do  mapM_ doesDirectoryExist srcDirs
              return project

        -- Right project@(App appInfo@(AppInfo _ srcDirs _ _ _ _)) ->
        --   do  mapM_ doesDirectoryExist srcDirs
        --   -- LAMDERA remove me, this works but is too brutal
        --       return $ App $ appInfo { _app_source_dirs = srcDirs ++ ["lamdera-stuff/alpha"] }
```
