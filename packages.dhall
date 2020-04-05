
let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.3-20190330/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.5-20191127/packages.dhall sha256:654e8427ff1f9830542f491623cd5d89b1648774a765520554f98f41d3d1b3b3

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
