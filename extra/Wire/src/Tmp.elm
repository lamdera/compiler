module Tmp exposing (..)

x = (Name {_author = "elm", _project = "browser"}
    ,Name {_name = "Browser"}
    ,Name {_name = "External"}
    ,Ctor (ZeroBased 1) 1)

a =
  ( Global (Canonical { package_ = Name { author_ = "author", project_ = "project" }, module_ = Name { name_ = "Simple" } }) (Name { name_ = "floatconstant" }), Define (Float 0.19) (fromList []) )
a =
  ( Global (Canonical { package_ = Name { author_ = "author", project_ = "project" }, module_ = Name { name_ = "Simple" } }) (Name { name_ = "main" }), Define (Call (VarGlobal (Global (Canonical { package_ = Name { author_ = "elm", project_ = "browser" }, module_ = Name { name_ = "Browser" } }) (Name { name_ = "sandbox" }))) [ Record (fromList [ ( Name { name_ = "init" }, Unit ), ( Name { name_ = "update" }, Function [ Name { name_ = "_n0" }, Name { name_ = "model" } ] (VarLocal (Name { name_ = "model" })) ), ( Name { name_ = "view" }, Function [ Name { name_ = "m" } ] (Call (VarGlobal (Global (Canonical { package_ = Name { author_ = "elm", project_ = "html" }, module_ = Name { name_ = "Html" } }) (Name { name_ = "text" }))) [ Str "asdf" ]) ) ]) ]) (fromList [ Global (Canonical { package_ = Name { author_ = "elm", project_ = "browser" }, module_ = Name { name_ = "Browser" } }) (Name { name_ = "sandbox" }), Global (Canonical { package_ = Name { author_ = "elm", project_ = "html" }, module_ = Name { name_ = "Html" } }) (Name { name_ = "text" }), Global (Canonical { package_ = Name { author_ = "elm", project_ = "json" }, module_ = Name { name_ = "Json.Decode" } }) (Name { name_ = "succeed" }), Global (Canonical { package_ = Name { author_ = "elm", project_ = "kernel" }, module_ = Name { name_ = "Utils" } }) (Name { name_ = "$" }) ]) )
a =
  ( Global (Canonical { package_ = Name { author_ = "elm", project_ = "browser" }, module_ = Name { name_ = "Browser" } }) (Name { name_ = "Document" }), Define (Function [ Name { name_ = "title" }, Name { name_ = "body" } ] (Record (fromList [ ( Name { name_ = "body" }, VarLocal (Name { name_ = "body" }) ), ( Name { name_ = "title" }, VarLocal (Name { name_ = "title" }) ) ]))) (fromList []) )
a =
  ( Global (Canonical { package_ = Name { author_ = "elm", project_ = "browser" }, module_ = Name { name_ = "Browser" } }) (Name { name_ = "External" }), Ctor (ZeroBased 1) 1 )
a =
  ( Global (Canonical { package_ = Name { author_ = "elm", project_ = "browser" }, module_ = Name { name_ = "Browser" } }) (Name { name_ = "Internal" }), Ctor (ZeroBased 0) 1 )
a =
  ( Global (Canonical { package_ = Name { author_ = "elm", project_ = "browser" }, module_ = Name { name_ = "Browser" } }) (Name { name_ = "application" }), Define (VarKernel (Name { name_ = "Browser" }) (Name { name_ = "application" })) (fromList [ Global (Canonical { package_ = Name { author_ = "elm", project_ = "kernel" }, module_ = Name { name_ = "Browser" } }) (Name { name_ = "$" }) ]) )
a =
  ( Global (Canonical { package_ = Name { author_ = "elm", project_ = "browser" }, module_ = Name { name_ = "Browser" } }) (Name { name_ = "document" }), Define (VarKernel (Name { name_ = "Browser" }) (Name { name_ = "document" })) (fromList [ Global (Canonical { package_ = Name { author_ = "elm", project_ = "kernel" }, module_ = Name { name_ = "Browser" } }) (Name { name_ = "$" }) ]) )
a =
  ( Global (Canonical { package_ = Name { author_ = "elm", project_ = "browser" }, module_ = Name { name_ = "Browser" } }) (Name { name_ = "element" }), Define (VarKernel (Name { name_ = "Browser" }) (Name { name_ = "element" })) (fromList [ Global (Canonical { package_ = Name { author_ = "elm", project_ = "kernel" }, module_ = Name { name_ = "Browser" } }) (Name { name_ = "$" }) ]) )
a =
  ( Global (Canonical { package_ = Name { author_ = "elm", project_ = "browser" }, module_ = Name { name_ = "Browser" } }) (Name { name_ = "sandbox" }), Define (Function [ Name { name_ = "impl" } ] (Call (VarKernel (Name { name_ = "Browser" }) (Name { name_ = "element" })) [ Record (fromList [ ( Name { name_ = "init" }, Function [ Name { name_ = "_n0" } ] (Tuple (Access (VarLocal (Name { name_ = "impl" })) (Name { name_ = "init" })) (VarGlobal (Global (Canonical { package_ = Name { author_ = "elm", project_ = "core" }, module_ = Name { name_ = "Platform.Cmd" } }) (Name { name_ = "none" }))) Nothing) ), ( Name { name_ = "subscriptions" }, Function [ Name { name_ = "_n1" } ] (VarGlobal (Global (Canonical { package_ = Name { author_ = "elm", project_ = "core" }, module_ = Name { name_ = "Platform.Sub" } }) (Name { name_ = "none" }))) ), ( Name { name_ = "update" }, Function [ Name { name_ = "msg" }, Name { name_ = "model" } ] (Tuple (Call (Access (VarLocal (Name { name_ = "impl" })) (Name { name_ = "update" })) [ VarLocal (Name { name_ = "msg" }), VarLocal (Name { name_ = "model" }) ]) (VarGlobal (Global (Canonical { package_ = Name { author_ = "elm", project_ = "core" }, module_ = Name { name_ = "Platform.Cmd" } }) (Name { name_ = "none" }))) Nothing) ), ( Name { name_ = "view" }, Access (VarLocal (Name { name_ = "impl" })) (Name { name_ = "view" }) ) ]) ])) (fromList [ Global (Canonical { package_ = Name { author_ = "elm", project_ = "core" }, module_ = Name { name_ = "Platform.Cmd" } }) (Name { name_ = "none" }), Global (Canonical { package_ = Name { author_ = "elm", project_ = "core" }, module_ = Name { name_ = "Platform.Sub" } }) (Name { name_ = "none" }), Global (Canonical { package_ = Name { author_ = "elm", project_ = "kernel" }, module_ = Name { name_ = "Browser" } }) (Name { name_ = "$" }), Global (Canonical { package_ = Name { author_ = "elm", project_ = "kernel" }, module_ = Name { name_ = "Utils" } }) (Name { name_ = "$" }) ]) )
a =
  ( Global (Canonical { package_ = Name { author_ = "elm", project_ = "browser" }, module_ = Name { name_ = "Browser.AnimationManager" } }) (Name { name_ = "$fx$" }), Manager Sub )
