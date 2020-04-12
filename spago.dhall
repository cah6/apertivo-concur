{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "purescript-concur-starter"
, dependencies =
    [ "aff"
    , "aff-promise"
    , "concur-core"
    , "concur-react"
    , "console"
    , "effect"
    , "simple-json"
    , "react-mui"
    ]
, packages =
    ./packages.dhall
}
