module Main where

import Custom.Prelude

import Concur as C
import Data.Array (filter, range, zip)
import Data.Enum (enumFromTo)
import Data.TSCompat.React (unsafeCreateElement)
import Foreign.MaterialUI (createMuiTheme)
import Foreign.MaterialUI as M
import MaterialUI.Styles (muiThemeProviderClass)
import React.DOM.Props (unsafeFromPropsArray)
import Types (HappyHour, staticHappyHours)

-- import Foreign.Firebase as Firebase
-- import Foreign.GoogleMaps as GoogleMaps



myTheme = createMuiTheme
  { spacing: 8
  , palette:
    { primary:
      { main: "#11cb5f"
      }
    }
  }

concurThemeProvider = C.el' (unsafeCreateElement muiThemeProviderClass <<< unsafeFromPropsArray)
  [ C.unsafeMkProp "theme" myTheme ]


main :: Effect Unit
main = C.runWidgetInDom "root" root


root :: forall a. C.Widget C.HTML a
root = M.pickerProvider $ concurThemeProvider $ pure $ do
  _ <- tableView
  -- _ <- C.button [ C.onClick, C.className "button" ] [ C.text "Say Hello" ]
  -- xs <- liftAff getCollectionTyped <|> spinner [ C.size 300 ] []
  -- res <- autocomplete
  --   [ C.unsafeMkProp "types" [ "establishment" ]
  --   , C.unsafeMkPropHandler "onPlaceSelected"
  --   , C.className "autocomplete"
  --   ]
  --   [ ]
  -- liftEffect $ log $ "got autocomplete result: " <> res
  xs <- pure staticHappyHours
  -- _ <- C.text $ show xs
  -- _ <- C.div [C.className "buttons has-addons is-centered"] [ dayOfWeekBtns ]
  -- _ <- mkGoogleMap xs
  C.text "We never get here"


tableView :: forall a. C.Widget C.HTML a
tableView = C.div [] $
  [ filterAreaMui
  , table
  ]


filterAreaMui :: forall a. C.Widget C.HTML a
filterAreaMui = do
  res <- M.grid
    [ C.unsafeMkProp "container" true
    ]
    [ box1 [] $ M.textField [ C.label "Restaurant", C.variant "outlined" ] []
    , box1 [] $ M.textField [ C.label "Description contains", C.variant "outlined" ] []
    , box1 [ C.width "125" ] $ M.timePicker [ C.label "Active during" ] []
    , M.box [ mx "auto" ] []
    , box1 [] $ M.button [ C.color "primary" ] [ C.text "Create New" ]
    -- M.box [ C.unsafeMkProp "ml" "auto" ] $ pure $
    ]
  filterAreaMui

-- Box with sensible defaults
box1 props child = M.box ([ mx 1, my "auto" ] <> props) [child]

mx = C.unsafeMkProp "mx"

my = C.unsafeMkProp "my"



table :: forall a. C.Widget C.HTML a
table = box1 [ my 2 ] $ M.paper [ elevation 2 ] $ pure $ M.table
  [ ]
  [ M.thead []
    [ M.trow [] $ mapFlipped columns $ (\c -> M.tcell [] [ C.text c ])
    ]
  , M.tbody [] $
    hhToRow <$> staticHappyHours
  ]

elevation = C.unsafeMkProp "elevation"

daysOrdered :: Array Weekday
daysOrdered = [ Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday]

hhToRow :: forall a. HappyHour -> C.Widget C.HTML a
hhToRow hh = M.trow []
  [ M.tcell [] [ C.text hh.restaurant ]
  , cell (show hh.schedule)
  , cell (show hh.schedule)
  , cell "Some description"
  ]
  where
  cell t = M.tcell [] [ C.text t ]


columns :: Array String
columns = ["Restaurant", "Days", "Time", "Description"]



------------------------------------------------------------------------------
dayOfWeekBtns :: C.Widget C.HTML (Array Weekday)
dayOfWeekBtns = do
  let days :: Array (Tuple Weekday Boolean)
      days = zip (enumFromTo Monday Sunday) (false <$ range 1 7)
  enabledDays <- C.andd $ map singleDayBtn days
  -- it never returns
  pure $ map fst $ filter (\tup -> snd tup == true) enabledDays


singleDayBtn :: (Tuple Weekday Boolean) -> C.Widget C.HTML (Tuple Weekday Boolean)
singleDayBtn (Tuple dow initial) = do
  let onClass = "button is-rounded is-selected is-active"
      offClass = "button is-rounded"
      currClass = if initial then onClass else offClass
  clicked <- C.button [ C.onClick, C.className currClass ] [ C.text (show dow) ]
  liftEffect $ log $ "clicked! " <> show currClass
  singleDayBtn (Tuple dow (not initial))


wrapClass :: forall a. String -> Array (C.Widget C.HTML a) -> C.Widget C.HTML a
wrapClass clazz = C.div [ C.className clazz ]

wrapClass1 :: forall a. String -> C.Widget C.HTML a -> C.Widget C.HTML a
wrapClass1 clazz child = C.div [ C.className clazz ] [ child ]
