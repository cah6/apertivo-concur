
let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.3-20190330/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.5-20191127/packages.dhall sha256:654e8427ff1f9830542f491623cd5d89b1648774a765520554f98f41d3d1b3b3

let overrides = {=}

let additions =
  { react-mui =
      { dependencies =
          [ "prelude"
          , "react"
          , "react-dom"
          , "tscompat"
          , "typelevel-prelude"
          ]
      , repo =
          "https://github.com/doolse/purescript-react-mui.git"
      , version =
          "v3.9.313"
      }
  , tscompat =
      { dependencies =
          [ "prelude"
          , "react"
          , "typelevel-prelude"
          ]
      , repo =
          "https://github.com/doolse/purescript-tscompat.git"
      , version =
          "v1.0.1"
      }
  -- for option
  , codec-argonaut =
      { dependencies =
          [ "argonaut-core",
            "codec",
            "generics-rep",
            "variant",
            "ordered-collections",
            "type-equality"
          ]
      , repo =
          "https://github.com/garyb/purescript-codec-argonaut.git"
      , version =
          "v7.1.0"
      }
  , codec =
      { dependencies =
          [ "transformers",
            "profunctor"
          ]
      , repo =
          "https://github.com/garyb/purescript-codec.git"
      , version =
          "v3.0.0"
      }
  , option =
      { dependencies =
          [ "prelude",
            "argonaut-codecs",
            "argonaut-core",
            "codec",
            "codec-argonaut",
            "argonaut",
            "either",
            "foreign",
            "foreign-object",
            "lists",
            "maybe",
            "profunctor",
            "prelude",
            "record",
            "simple-json",
            "transformers",
            "tuples",
            "type-equality",
            "unsafe-coerce"
          ]
      , repo =
          "https://github.com/joneshf/purescript-option.git"
      , version =
          "v2.1.0"
      }
  }

in  upstream // overrides // additions
