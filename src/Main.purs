module Main where

import Control.MultiAlternative
import Control.ShiftMap
import Custom.Prelude

import Concur (p)
import Concur as C
import Control.Monad.Except (ExceptT(..), runExcept, runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..))
import Data.Array (catMaybes, concatMap, elem, filter, length, mapWithIndex, range, sortWith, zip)
import Data.DateTime (time)
import Data.DateTime.Instant (toDateTime)
import Data.Enum (enumFromTo, fromEnum, toEnum)
import Data.Generic.Rep.Enum (genericToEnum)
import Data.Int (round, toNumber)
import Data.JSDate (JSDate, getHours, getMinutes, getTimezoneOffset, getUTCHours, getUTCMinutes, jsdate, jsdateLocal, readDate)
import Data.JSDate as JSDate
import Data.Map (Map, fromFoldable)
import Data.Time (Time(..), hour, minute)
import Data.Time as Time
import Data.Time.Duration as Duration
import Data.UUID as UUID
import Effect.Now (nowTime, now, nowDateTime)
import Foreign.Firebase as Firebase
import Foreign.Geolocation (Coords)
import Foreign.Geolocation as Geo
import Foreign.MaterialUI as M
import Foreign.Spinner as Spinner
import MaterialUI.Styles (createMuiTheme)
import MaterialUI.Theme (Theme)
import Math (trunc)
import Types (HappyHour, Weekday(..), abbreviate, daysOrdered, staticHappyHours)
import Unsafe.Coerce (unsafeCoerce)
import Utils.Time (jsDateToTime, localTimeToUtcJsDate) as Time

-- import Foreign.Firebase as Firebase
-- import Foreign.GoogleMaps as GoogleMaps


globalTheme :: Theme
globalTheme = createMuiTheme
  { spacing: 8
  , palette:
    { type: "light"
    , primary: {
        main: "#f06292"
      }
    , secondary: {
        main: "#7986cb"
      }
    }
  }

main :: Effect Unit
main = C.runWidgetInDom "root" root


root :: forall a. C.Widget C.HTML a
root = M.pickerProvider $ M.themeProvider globalTheme $ C.div' $ pure $ do
  -- orr this with a loader & nice message
  coords <- liftAff Geo.getLocation
    <|> loadingView "Please allow location access."
  liftEffect $ log $ show coords

  -- userCreds <- liftAff Firebase.signInWithPopup
  --   <|> loadingView "Signing in..."
  -- liftEffect $ log $ "Got user: " <> show userCreds

  filterValues <- liftEffect (initFilterValues coords)
  _ <- tableView {coords} filterValues
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
  C.text "We never get here"

loadingView :: forall a. String -> C.Widget C.HTML a
loadingView msg = M.grid
  [ C.unsafeMkProp "container" true
  , C.unsafeMkProp "direction" "column"
  , C.unsafeMkProp "justify" "center"
  , C.unsafeMkProp "alignItems" "center"
  ]
  [ Spinner.spinner [] []
  , M.typography1 [ C.variant "overline" ] $ C.text msg
  ]


initFilterValues :: Geo.Coords -> Effect FilterValues
initFilterValues coords = do
  nowDateTimeVal <- liftEffect $ JSDate.now
  time <- liftEffect $ Time.jsDateToTime nowDateTimeVal
  dayNum <- JSDate.getDay nowDateTimeVal
  let dayVal = maybe Tuesday identity $ genericToEnum (round dayNum)
  pure {timeOfDay: time, day: dayVal, coords }


tableView :: forall a. UserInfo -> FilterValues -> C.Widget C.HTML a
tableView user filterValues = do
  newFilterValues <- C.div [] $
    [ filterAreaMui filterValues
    , table user filterValues
    ]
  tableView user newFilterValues


type FilterValues =
  { timeOfDay :: Time
  , day :: Weekday
  , coords :: Geo.Coords
  }


filterAreaMui :: FilterValues -> C.Widget C.HTML FilterValues
filterAreaMui st = do
  newSt <- M.grid
    [ C.unsafeMkProp "container" true
    ]
    [ box1 [] $ M.tooltip1 [ C.title "Restaurant name, city, description, etc."] $
        M.textField [ C.label "Quick filter", C.variant "outlined" ] []
    , st { timeOfDay = _ } <$> box1 [ C.width "125"] (timePicker st.timeOfDay)
    , st { day = _ } <$> box1 [] (daySelect st.day)
    , st <$ box1 [] (distanceSelect 1.0)
    , M.box [ C.mx "auto" ] [] -- to push next to right
    , box1 [] $ M.button [ C.color "primary" ] [ C.text "Create New" ]
    ]
  filterAreaMui newSt


timePicker :: Time -> C.Widget C.HTML Time
timePicker localTime = do
  widgetTime <- liftEffect $ Time.localTimeToUtcJsDate localTime
  fgn <- M.timePicker
    [ C.label "Active during"
    , C.unsafeMkPropHandler "onChange"
    , C.unsafeMkProp "minutesStep" 15
    , C.unsafeMkProp "value" widgetTime
    ]
    []
  -- try to read the timestamp, reloading widget in place if it's bad (somehow)
  case runExcept (readDate fgn) of
    Left e -> timePicker localTime
    Right epoch -> liftEffect $ Time.jsDateToTime epoch


daySelect :: Weekday -> C.Widget C.HTML Weekday
daySelect selected = do
  newDay <- M.formControl []
    [ M.inputLabel [] [ C.text "On day" ]
    , M.select
      [ C.value_ selected
      -- this isn't THAT unsafe, since react will give us back whatever we give it
      , unsafeCoerce <<< C.unsafeTargetValue <$> C.onChange
      ] $ mapFlipped daysOrdered $ \day -> M.menuItem
          [ C.value_ day
          ]
          [ C.div [] [ C.text $ show day] ]
    ]
  daySelect newDay


distanceSelect :: Number -> C.Widget C.HTML Number
distanceSelect selected = do
  newDistance <- M.formControl []
    [ M.inputLabel [] [ C.text "Within" ]
    , M.select
      [ C.value_ selected
      , unsafeCoerce <<< C.unsafeTargetValue <$> C.onChange
      ] $ mapFlipped distanceOptions $ \(Tuple distance txt) -> M.menuItem
          [ C.value_ distance
          ]
          [ C.div [] [ C.text txt] ]
    ]
  distanceSelect newDistance

distanceOptions :: Array (Tuple Number String)
distanceOptions =
  [ Tuple (div 4.0 17.0) "4 blocks"
  , Tuple 1.0 "1 mi"
  , Tuple 2.0 "2 mi"
  , Tuple 5.0 "5 mi"
  , Tuple 10.0 "10 mi"
  , Tuple 25.0 "25 mi"
  ]


table :: forall a. UserInfo -> FilterValues -> C.Widget C.HTML a
table user fv = box1 [ C.my 2 ] $ M.paper1 [ C.elevation 2 ] $ M.table
  [ ]
  [ M.thead []
    [ M.trow [] $ mapFlipped columns $ (\c -> M.tcell [] [ C.text c ])
    ]
  , M.tbody [] $ concatMap hhToRow hhs
  ]
  where
  hhs = sortWith _.distance (mkRelative user <$> staticHappyHours)

mkRelative :: UserInfo -> HappyHour -> RelativeHappyHour
mkRelative user hh = {distance: Geo.distance user.coords hh.latLng, hh: hh }

type UserInfo =
  { coords :: Coords
  }

type RelativeHappyHour =
  { distance :: Number
  , hh :: HappyHour
  }

-- filterAndSort :: FilterValues -> Array HappyHour -> Array HappyHour
-- filterAndSort fv xs =


-- filterHappyHour :: FilterValues -> HappyHour -> Boolean
-- filterHappyHour fv hh =

hhToRow :: forall a. RelativeHappyHour -> Array (C.Widget C.HTML a)
hhToRow rel = (flip mapWithIndex) hh.schedule $ \i schedule -> M.trow [] $ catMaybes
  [ condCell i $ M.tcell1 [ C.unsafeMkProp "rowSpan" numSchedules ] $
      M.link [ C.variant "subtitle2", C.href hh.link ] [ C.text hh.restaurant ]
  , condCell i $ M.tcell1 [ C.unsafeMkProp "rowSpan" numSchedules ] $
      C.text $ show (trunc (100.0 * rel.distance) / 100.0) <> " mi"
  , Just $ M.tcell1' $ weekDisplay schedule.days
  , Just $ M.tcell1' $ C.text schedule.time
  , Just $ M.tcell1' $ C.text schedule.scheduleDescription
  ]
  where
  hh = rel.hh
  numSchedules = length hh.schedule
  condCell i cell = if i == 0 then Just cell else Nothing

weekDisplay :: forall a. Array Weekday -> C.Widget C.HTML a
weekDisplay daysActive = M.buttonGroup
  [ C.variant "text"
  ]
  ((\s -> dayDisplay s (s `elem` daysActive)) <$> daysOrdered)



dayDisplay :: forall a. Weekday -> Boolean -> C.Widget C.HTML a
dayDisplay day isOn = M.button
  (onProp <> [C.unsafeMkProp "component" "div"])
  [ C.text (abbreviate day) ]
  where
  onProp = if isOn then [ C.color "primary" ] else []

columns :: Array String
columns = ["Restaurant", "Distance", "Days", "Time", "Description"]


-- Box with sensible defaults
box1 :: forall a. Array (C.ReactProps a) -> C.Widget C.HTML a -> C.Widget C.HTML a
box1 props child = M.box ([ C.mx 1, C.my "auto" ] <> props) [child]
