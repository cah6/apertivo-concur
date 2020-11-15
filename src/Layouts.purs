module Layouts where

import Custom.Prelude
import Option (Option, fromRecord, get)
import Concur.Extended as C
import DOM (installStyleInHead)
import Data.Rational as Rational
import Foreign.Functions (attachReelObserver)
import React.Ref (NativeNode, Ref, createNodeRef, fromEffect, getCurrentRef)

------------------------------------------------------------------------------
-- | Icon section
type IconConfig'
  = ( space :: Maybe String
    , label :: Maybe String
    )

type IconConfig
  = Record IconConfig'

defaultIconConfig :: IconConfig
defaultIconConfig =
  { space: Nothing
  , label: Nothing
  }

icon :: forall a. C.Widget C.HTML a -> C.Widget C.HTML a
icon child = icon_ (fromRecord {}) child

icon_ :: forall a. Option IconConfig' -> C.Widget C.HTML a -> C.Widget C.HTML a
icon_ overrides child = do
  _ <- liftEffect $ installStyleInHead id (intercalate "" innerHTML)
  el <-
    C.el' (C.mkDOM (C.IsDynamic false) "icon-l")
      [ C._data { "i": id }
      ]
      [ child ]
  pure el
  where
  space = fromMaybe defaultIconConfig.space (get (SProxy :: _ "space") overrides)

  label = fromMaybe defaultIconConfig.label (get (SProxy :: _ "label") overrides)

  id = "Icon-" <> show space <> show label

  addAttributes = case label of
    Nothing -> []
    Just label_ -> [ C.role "img", C.unsafeMkProp "aria-label" label_ ]

  innerHTML = case space of
    Nothing -> []
    Just space_ ->
      [ dataId id <> " { display: inline-flex; align-items: baseline; }"
      , dataId id <> " > svg { margin-inline-end: " <> space_ <> "; }"
      ]

------------------------------------------------------------------------------
-- | Imposter section
type ImposterConfig'
  = ( breakout :: Boolean
    , margin :: String
    , fixed :: Boolean
    )

type ImposterConfig
  = Record ImposterConfig'

defaultImposterConfig :: ImposterConfig
defaultImposterConfig =
  { breakout: false
  , margin: "0px"
  , fixed: false
  }

imposter :: forall a. C.Widget C.HTML a -> C.Widget C.HTML a
imposter child = imposter_ (fromRecord {}) child

imposter_ :: forall a. Option ImposterConfig' -> C.Widget C.HTML a -> C.Widget C.HTML a
imposter_ overrides child = do
  _ <- liftEffect $ installStyleInHead id (intercalate "" innerHTML)
  el <-
    C.el' (C.mkDOM (C.IsDynamic false) "imposter-l")
      [ C._data { "i": id }
      ]
      [ child ]
  pure el
  where
  breakout = fromMaybe defaultImposterConfig.breakout (get (SProxy :: _ "breakout") overrides)

  margin' = fromMaybe defaultImposterConfig.margin (get (SProxy :: _ "margin") overrides)

  margin = fixZero margin'

  fixed = fromMaybe defaultImposterConfig.fixed (get (SProxy :: _ "fixed") overrides)

  id = "Imposter-" <> show breakout <> margin <> show fixed

  maxDim = "calc(100% - (" <> margin <> " * 2));"

  breakoutSection = if breakout then "" else "max-width: " <> maxDim <> "max-height: " <> maxDim <> "overflow: auto;"

  fixedSection = if fixed then "position: fixed;" else ""

  innerHTML =
    [ dataId id <> " { " <> breakoutSection <> fixedSection <> " }"
    ]

------------------------------------------------------------------------------
-- | Frame section
type FrameConfig'
  = ( ratio :: Rational.Rational
    )

type FrameConfig
  = Record FrameConfig'

defaultFrameConfig :: FrameConfig
defaultFrameConfig =
  { ratio: 16 Rational.% 9
  }

frame :: forall a. C.Widget C.HTML a -> C.Widget C.HTML a
frame child = frame_ (fromRecord {}) child

frame_ :: forall a. Option FrameConfig' -> C.Widget C.HTML a -> C.Widget C.HTML a
frame_ overrides child = do
  _ <- liftEffect $ installStyleInHead id (intercalate "" innerHTML)
  el <-
    C.el' (C.mkDOM (C.IsDynamic false) "frame-l")
      [ C._data { "i": id }
      , C.style
          { "--numerator": numerator
          , "--denominator": denominator
          }
      ]
      [ child ]
  pure el
  where
  ratio = fromMaybe defaultFrameConfig.ratio (get (SProxy :: _ "ratio") overrides)

  numerator = Rational.numerator ratio

  denominator = Rational.denominator ratio

  id = "Frame-" <> show ratio

  innerHTML = []

------------------------------------------------------------------------------
-- | Cover section
type CoverConfig'
  = ( centered :: String
    , space :: String
    , minHeight :: String
    , noPad :: Boolean
    )

type CoverConfig
  = Record CoverConfig'

defaultCoverConfig :: CoverConfig
defaultCoverConfig =
  { centered: "h1"
  , space: "var(--s1)"
  , minHeight: "100vh"
  , noPad: false
  }

cover :: forall a. Array (C.Widget C.HTML a) -> C.Widget C.HTML a
cover children = cover_ (fromRecord {}) children

cover_ :: forall a. Option CoverConfig' -> Array (C.Widget C.HTML a) -> C.Widget C.HTML a
cover_ overrides children = do
  _ <- liftEffect $ installStyleInHead id (intercalate "" innerHTML)
  el <-
    C.el' (C.mkDOM (C.IsDynamic false) "cover-l")
      [ C._data { "i": id }
      , C.style
          { "--centered": centered
          , "--space": space
          , "--minHeight": minHeight
          , "--padding": padding
          }
      ]
      children
  pure el
  where
  centered = fromMaybe defaultCoverConfig.centered (get (SProxy :: _ "centered") overrides)

  space = fromMaybe defaultCoverConfig.space (get (SProxy :: _ "space") overrides)

  minHeight = fromMaybe defaultCoverConfig.minHeight (get (SProxy :: _ "minHeight") overrides)

  noPad = fromMaybe defaultCoverConfig.noPad (get (SProxy :: _ "noPad") overrides)

  padding = if noPad then "0" else space

  id = "Cover-" <> centered <> space <> minHeight <> show noPad

  innerHTML =
    [ dataId id <> " > :first-child:not(" <> centered <> ") { margin-top: 0 }"
    , dataId id <> " > :last-child:not(" <> centered <> ") { margin-bottom: 0 }"
    , dataId id <> " > " <> centered <> "{ margin-top: auto; margin-bottom: auto; }"
    ]

------------------------------------------------------------------------------
-- | Switcher section
type SwitcherConfig'
  = ( threshold :: String
    , space :: String
    , limit :: Int
    )

type SwitcherConfig
  = Record SwitcherConfig'

defaultSwitcherConfig :: SwitcherConfig
defaultSwitcherConfig =
  { threshold: "var(--measure)"
  , space: "var(--s1)"
  , limit: 4
  }

switcher :: forall a. Array (C.Widget C.HTML a) -> C.Widget C.HTML a
switcher child = switcher_ (fromRecord {}) child

switcher_ :: forall a. Option SwitcherConfig' -> Array (C.Widget C.HTML a) -> C.Widget C.HTML a
switcher_ overrides children = do
  _ <- liftEffect $ installStyleInHead id (intercalate "" innerHTML)
  el <-
    C.el' (C.mkDOM (C.IsDynamic false) "switcher-l")
      [ C._data { "i": id }
      , C.style
          { "--threshold": threshold
          , "--space": space
          , "--limit": limit
          }
      ]
      [ C.div' children ]
  pure el
  where
  threshold = fromMaybe defaultSwitcherConfig.threshold (get (SProxy :: _ "threshold") overrides)

  space' = fromMaybe defaultSwitcherConfig.space (get (SProxy :: _ "space") overrides)

  space = if space' == "0" then "0px" else space'

  limit = fromMaybe defaultSwitcherConfig.limit (get (SProxy :: _ "limit") overrides)

  id = "Switcher-" <> threshold <> space <> show limit

  nthLastChild = ":nth-last-child(n+" <> show (limit + 1) <> ")"

  selector = dataId id <> " > * > " <> nthLastChild

  innerHTML =
    [ selector <> "," <> selector <> " ~ * { flex-basis: 100% }"
    ]

------------------------------------------------------------------------------
-- | Sidebar section
type SidebarConfig'
  = ( side :: Side
    , sideWidth :: Maybe String
    , contentMin :: Int -- could really be natural, whatever
    , space :: String
    , noStretch :: Boolean
    )

type SidebarConfig
  = Record SidebarConfig'

defaultSidebarConfig :: SidebarConfig
defaultSidebarConfig =
  { side: Side_Left
  , sideWidth: Nothing
  , contentMin: 50
  , space: "var(--s1)"
  , noStretch: false
  }

data Side
  = Side_Left
  | Side_Right

instance sideShow :: Show Side where
  show a = case a of
    Side_Left -> "Left"
    Side_Right -> "Right"

sidebar :: forall a. C.Widget C.HTML a -> C.Widget C.HTML a
sidebar child = sidebar_ (fromRecord {}) child

sidebar_ :: forall a. Option SidebarConfig' -> C.Widget C.HTML a -> C.Widget C.HTML a
sidebar_ overrides child = do
  _ <- liftEffect $ installStyleInHead id (intercalate "" innerHTML)
  el <-
    C.el' (C.mkDOM (C.IsDynamic false) "sidebar-l")
      [ C._data { "i": id }
      ]
      [ child ]
  pure el
  where
  side = fromMaybe defaultSidebarConfig.side (get (SProxy :: _ "side") overrides)

  sideWidth = fromMaybe defaultSidebarConfig.sideWidth (get (SProxy :: _ "sideWidth") overrides)

  contentMin' = fromMaybe defaultSidebarConfig.contentMin (get (SProxy :: _ "contentMin") overrides)

  contentMin = show contentMin' <> "%"

  space' = fromMaybe defaultSidebarConfig.space (get (SProxy :: _ "space") overrides)

  space = if space' == "0" then "0px" else space'

  noStretch = fromMaybe defaultSidebarConfig.noStretch (get (SProxy :: _ "noStretch") overrides)

  showSideWidth = maybe "" show sideWidth

  id = "Sidebar-" <> show side <> showSideWidth <> contentMin <> space <> show noStretch

  negMarginLine = "margin: calc(" <> space <> " / 2 * -1);"

  noStretchLine = if noStretch then "align-items: flex-start;" else ""

  marginLine = "margin: calc(" <> space <> " / 2);"

  sideWidthLine = case sideWidth of
    Nothing -> ""
    Just sideWidth_ -> "flex-basis: " <> sideWidth_ <> ";"

  -- | If our sidebar is on the left, this is the last child.
  notSidebar = case side of
    Side_Left -> ":last-child"
    Side_Right -> ":first-child"

  minWidthLine = "min-width: calc(" <> contentMin <> " - " <> space <> ");"

  innerHTML =
    [ dataId id <> " > * { " <> negMarginLine <> noStretchLine <> "}"
    , dataId id <> " > * > * { " <> marginLine <> sideWidthLine <> "}"
    , dataId id <> " > * > " <> notSidebar <> " { flex-basis: 0; flex-grow: 999; " <> minWidthLine <> " }"
    ]

------------------------------------------------------------------------------
-- | Cluster section
type ClusterConfig'
  = ( justify :: String -- maybe sum type
    , align :: String
    , space :: String
    )

type ClusterConfig
  = Record ClusterConfig'

defaultClusterConfig :: ClusterConfig
defaultClusterConfig =
  { justify: "center"
  , align: "center"
  , space: "var(--s1)"
  }

cluster :: forall a. (Array (C.Widget C.HTML a)) -> C.Widget C.HTML a
cluster children = cluster_ (fromRecord {}) children

cluster_ :: forall a. Option ClusterConfig' -> (Array (C.Widget C.HTML a)) -> C.Widget C.HTML a
cluster_ overrides children = do
  _ <- liftEffect $ installStyleInHead id (intercalate "" innerHTML)
  el <-
    C.el' (C.mkDOM (C.IsDynamic false) "cluster-l")
      [ C._data { "i": id }
      , C.style
          { "--space": space
          , "--justify": justify
          , "--align": align
          }
      ]
      children
  pure el
  where
  justify = fromMaybe defaultClusterConfig.justify (get (SProxy :: _ "justify") overrides)

  align = fromMaybe defaultClusterConfig.align (get (SProxy :: _ "align") overrides)

  space = fromMaybe defaultClusterConfig.space (get (SProxy :: _ "space") overrides)

  id = "Cluster-" <> justify <> align <> space

  justifyLine = "justify-content: " <> justify <> ";"

  alignLine = "align-items: " <> align <> ";"

  innerHTML = []

------------------------------------------------------------------------------
-- | Center section
type CenterConfig'
  = ( max :: String
    , andText :: Boolean
    , gutters :: Maybe String
    , intrinsic :: Boolean
    , classes :: Array String
    )

type CenterConfig
  = Record CenterConfig'

defaultCenterConfig :: CenterConfig
defaultCenterConfig =
  { max: "var(--measure)"
  , andText: false
  , gutters: Nothing
  , intrinsic: false
  , classes: []
  }

center :: forall a. (Array (C.Widget C.HTML a)) -> C.Widget C.HTML a
center children = center_ (fromRecord {}) children

center_ :: forall a. Option CenterConfig' -> (Array (C.Widget C.HTML a)) -> C.Widget C.HTML a
center_ overrides children = do
  _ <- liftEffect $ installStyleInHead id (intercalate "" innerHTML)
  el <-
    C.el' (C.mkDOM (C.IsDynamic false) "center-l")
      [ C._data { "i": id }
      , C.style
          { "--max": max
          }
      , C.className $ "hero"
      ]
      children
  pure el
  where
  max = fromMaybe defaultCenterConfig.max (get (SProxy :: _ "max") overrides)

  andText = fromMaybe defaultCenterConfig.andText (get (SProxy :: _ "andText") overrides)

  gutters = fromMaybe defaultCenterConfig.gutters (get (SProxy :: _ "gutters") overrides)

  intrinsic = fromMaybe defaultCenterConfig.intrinsic (get (SProxy :: _ "intrinsic") overrides)

  classes = fromMaybe defaultCenterConfig.classes (get (SProxy :: _ "classes") overrides)

  showGutters = maybe "" show gutters

  id = "Center-" <> max <> show andText <> showGutters <> show intrinsic

  paddingLine = case gutters of
    Nothing -> ""
    Just gutter -> "padding-left: " <> gutter <> "; padding-right: " <> gutter <> ";"

  textAlignLine = if andText then "text-align: center;" else ""

  intrinsicLines = if intrinsic then "display: flex; flex-direction: column; align-items: center;" else ""

  innerHTML =
    [ dataId id <> "{ " <> paddingLine <> textAlignLine <> intrinsicLines <> "}"
    ]

------------------------------------------------------------------------------
-- | Grid section
type GridConfig'
  = ( min :: String
    , space :: String
    )

type GridConfig
  = Record GridConfig'

defaultGridConfig :: GridConfig
defaultGridConfig =
  { min: "250px"
  , space: "var(--s1)"
  }

grid :: forall a. (Array (C.Widget C.HTML a)) -> C.Widget C.HTML a
grid children = grid_ (fromRecord {}) children

grid_ :: forall a. Option GridConfig' -> (Array (C.Widget C.HTML a)) -> C.Widget C.HTML a
grid_ overrides children = do
  _ <- liftEffect $ installStyleInHead id (intercalate "" innerHTML)
  el <-
    C.el' (C.mkDOM (C.IsDynamic false) "grid-l")
      [ C._data { "i": id }
      ]
      children
  pure el
  where
  min = fromMaybe defaultGridConfig.min (get (SProxy :: _ "min") overrides)

  space = fromMaybe defaultGridConfig.space (get (SProxy :: _ "space") overrides)

  id = "Grid-" <> min <> space

  minMin100 = "min(" <> min <> ", 100%)"

  gridTempCol = "grid-template-columns: repeat(auto-fill, minmax(" <> minMin100 <> ", 1fr));"

  innerHTML =
    [ dataId id <> "{ grid-gap: " <> space <> "; }"
    , "@supports (width: " <> minMin100 <> ") { " <> dataId id <> " { " <> gridTempCol <> " } }"
    ]

------------------------------------------------------------------------------
-- | Reel section
type ReelConfig'
  = ( itemWidth :: String
    , space :: String
    , height :: String
    , noBar :: Boolean
    )

type ReelConfig
  = Record ReelConfig'

defaultReelConfig :: ReelConfig
defaultReelConfig =
  { itemWidth: "auto"
  , space: "var(--s0)"
  , height: "auto"
  , noBar: false
  }

reel :: forall a. (Array (C.Widget C.HTML a)) -> C.Widget C.HTML a
reel children = reel_ (fromRecord {}) children

reel_ :: forall a. Option ReelConfig' -> (Array (C.Widget C.HTML a)) -> C.Widget C.HTML a
reel_ overrides children = do
  _ <- liftEffect $ installStyleInHead id (intercalate "" innerHTML)
  thisRef <- liftEffect $ createNodeRef
  el <-
    C.el' (C.mkDOM (C.IsDynamic false) "reel-l")
      [ C._data { "i": id }
      , C.ref (fromEffect setupReel)
      , C.style
          { "--itemWidth": itemWidth
          , "--height": height
          , "--space": space
          }
      ]
      children
  pure el
  where
  itemWidth = fromMaybe defaultReelConfig.itemWidth (get (SProxy :: _ "itemWidth") overrides)

  space = fromMaybe defaultReelConfig.space (get (SProxy :: _ "space") overrides)

  height = fromMaybe defaultReelConfig.height (get (SProxy :: _ "height") overrides)

  noBar = fromMaybe defaultReelConfig.noBar (get (SProxy :: _ "noBar") overrides)

  id = "Reel-" <> itemWidth <> space <> height <> show noBar

  overflowingLine =
    if noBar then
      dataId id <> ".overflowing { }"
    else
      dataId id <> ".overflowing { padding-bottom: " <> space <> "; }"

  noBarLines =
    if noBar then
      dataId id <> "{ scrollbar-width: none; } " <> dataId id <> "::-webkit-scrollbar { display: none; }"
    else
      ""

  innerHTML =
    [ overflowingLine
    , noBarLines
    ]

setupReel :: Ref NativeNode -> Effect Unit
setupReel ref = do
  -- called when concur creates or re-creates a node
  mNode <- getCurrentRef ref
  _ <- case mNode of
    Just nativeNode -> attachReelObserver nativeNode
    Nothing -> pure unit
  pure unit

------------------------------------------------------------------------------
-- | Box section
type BoxConfig'
  = ( padding :: String
    , borderWidth :: String
    , invert :: Boolean
    , classes :: Array String
    , id :: Maybe String
    )

type BoxConfig
  = Record BoxConfig'

defaultBoxConfig :: BoxConfig
defaultBoxConfig =
  { padding: "var(--s1)"
  , borderWidth: "var(--border-thin)"
  , invert: false
  , classes: []
  , id: Nothing
  }

box :: forall a. (Array (C.Widget C.HTML a)) -> C.Widget C.HTML a
box children = box_ (fromRecord {}) children

box1 :: forall a. C.Widget C.HTML a -> C.Widget C.HTML a
box1 child = box_ (fromRecord {}) [ child ]

box1_ :: forall a. Option BoxConfig' -> C.Widget C.HTML a -> C.Widget C.HTML a
box1_ overrides child = box_ overrides [ child ]

box_ :: forall a. Option BoxConfig' -> (Array (C.Widget C.HTML a)) -> C.Widget C.HTML a
box_ overrides children = do
  _ <- liftEffect $ installStyleInHead id (intercalate "" innerHTML)
  C.el' (C.mkDOM (C.IsDynamic false) "box-l")
    [ C._data { "i": id }
    , C.unsafeMkProp "class" classes
    , C.unsafeMkProp "id" itemId
    ]
    children
  where
  padding = fromMaybe defaultBoxConfig.padding (get (SProxy :: _ "padding") overrides)

  borderWidth = fromMaybe defaultBoxConfig.borderWidth (get (SProxy :: _ "borderWidth") overrides)

  invert = fromMaybe defaultBoxConfig.invert (get (SProxy :: _ "invert") overrides)

  itemId = fromMaybe "" (join $ get (SProxy :: _ "id") overrides)

  classes' = fromMaybe defaultBoxConfig.classes (get (SProxy :: _ "classes") overrides)

  classes = intercalate " " classes'

  id = "Box-" <> padding <> borderWidth <> show invert

  invertLine = if invert then "background-color: var(--color-light); filter: invert(100%);" else ""

  innerHTML =
    [ dataId id <> "{ padding: " <> padding <> "; border: " <> borderWidth <> " solid; " <> invertLine <> "}"
    ]

------------------------------------------------------------------------------
-- | Stack section
type StackConfig'
  = ( space :: String
    , recursive :: Boolean
    , splitAfter :: Maybe Int
    )

type StackConfig
  = Record StackConfig'

defaultStackConfig :: StackConfig
defaultStackConfig =
  { space: "var(--s1)"
  , recursive: false
  , splitAfter: Nothing
  }

stack :: forall a. (Array (C.Widget C.HTML a)) -> C.Widget C.HTML a
stack children = stack_ (fromRecord {}) children

stack_ :: forall a. Option StackConfig' -> (Array (C.Widget C.HTML a)) -> C.Widget C.HTML a
stack_ overrides children = do
  _ <- liftEffect $ installStyleInHead id (intercalate "" innerHTML)
  C.el' (C.mkDOM (C.IsDynamic false) "stack-l")
    [ C._data { "i": id }
    , C.style
        { "--space": space
        }
    ]
    children
  where
  space = fromMaybe defaultStackConfig.space (get (SProxy :: _ "space") overrides)

  recursive = fromMaybe defaultStackConfig.recursive (get (SProxy :: _ "recursive") overrides)

  splitAfter = fromMaybe defaultStackConfig.splitAfter (get (SProxy :: _ "splitAfter") overrides)

  id = "Stack-" <> space <> show recursive <> maybe "" show splitAfter

  recursiveCombinator = if recursive then " " else " > "

  recursiveLine =
    if recursive then
      dataId id <> " " <> "* + * { margin-top: " <> space <> "}"
    else
      ""

  innerHTML = []

  -- innerHTML =
  --   [ recursiveLine
  --   ]
  --     <> conditionalHTML
  conditionalHTML = case splitAfter of
    Just n ->
      [ dataId id <> ":only-child{ height: 100%; }"
      , dataId id <> " > :nth-child(" <> show n <> ") { margin-bottom: auto; }"
      ]
    Nothing -> []

dataId :: String -> String
dataId id = "[data-i=\"" <> id <> "\"]"

fixZero :: String -> String
fixZero s = if s == "0" then "0px" else s
