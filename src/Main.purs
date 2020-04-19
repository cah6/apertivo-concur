module Main where

import Control.MultiAlternative
import Control.ShiftMap
import Custom.Prelude

import Concur (p)
import Concur as C
import Control.Monad.Except (ExceptT(..), runExcept, runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..))
import Data.Array (concatMap, elem, filter, range, zip)
import Data.DateTime (time)
import Data.DateTime.Instant (toDateTime)
import Data.Enum (enumFromTo, fromEnum, toEnum)
import Data.Int (round, toNumber)
import Data.JSDate (JSDate, getHours, getMinutes, getTimezoneOffset, getUTCHours, getUTCMinutes, jsdate, jsdateLocal, readDate)
import Data.JSDate as JSDate
import Data.Time (Time(..), hour, minute)
import Data.Time as Time
import Data.Time.Duration as Duration
import Data.UUID as UUID
import Effect.Now (nowTime, now, nowDateTime)
import Foreign.Firebase as Firebase
import Foreign.Geolocation as Geo
import Foreign.Spinner as Spinner
import Foreign.MaterialUI as M
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
  userCreds <- liftAff Firebase.signInWithRedirect
    <|> loadingView "Signing in..."

  liftEffect $ log $ "Got user: " <> show userCreds
  filterValues <- liftEffect (initFilterValues coords)
  _ <- tableView filterValues
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
  let nowDayVal = JSDate.getUTCDay nowDateTimeVal
  pure {timeOfDay: time, day: Monday, coords }


tableView :: forall a. FilterValues -> C.Widget C.HTML a
tableView filterValues = do
  newFilterValues <- C.div [] $
    [ filterAreaMui filterValues
    , table filterValues
    ]
  tableView newFilterValues


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
    [ box1 [] $ M.textField [ C.label "Quick filter", C.variant "outlined" ] []
    , (\newTime -> st { timeOfDay = newTime}) <$> box1 [ C.width "125"] (timePicker st.timeOfDay)
    , (\newDay -> st { day = newDay }) <$> box1 [] (daySelect Monday)
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
  -- give this widget an id nobody else has
  uuid <- liftEffect $ UUID.toString <$> UUID.genUUID
  newDay <- M.formControl []
    [ M.inputLabel [ C.id uuid ] [ C.text "On day" ]
    , M.select
      [ C.value_ selected
      -- this isn't THAT unsafe, since react will give us back whatever we give it
      , unsafeCoerce <<< C.unsafeTargetValue <$> C.onChange
      , C.unsafeMkProp "labelId" uuid
      ] $ mapFlipped daysOrdered $ \day -> M.menuItem
          [ C.value_ day
          ]
          [ C.div [] [ C.text $ show day] ]
    ]
  daySelect newDay

readWeekday :: String -> Weekday
readWeekday str = case str of
  "Sunday" -> Sunday
  "Monday" -> Monday
  "Tuesday" -> Tuesday
  "Wednesday" -> Wednesday
  "Thursday" -> Thursday
  "Friday" -> Friday
  "Saturday" -> Saturday
  _ -> Monday


table :: forall a. FilterValues -> C.Widget C.HTML a
table fv = box1 [ C.my 2 ] $ M.paper1 [ C.elevation 2 ] $ M.table
  [ ]
  [ M.thead []
    [ M.trow [] $ mapFlipped columns $ (\c -> M.tcell [] [ C.text c ])
    ]
  , M.tbody [] $ concatMap (hhToRow fv) staticHappyHours
  ]


hhToRow :: forall a. FilterValues -> HappyHour -> Array (C.Widget C.HTML a)
hhToRow fv hh = mapFlipped hh.schedule $ \schedule -> M.trow []
  [ M.tcell1' $ M.link [ C.color "initial", C.href hh.link ] [ C.text hh.restaurant ]
  , M.tcell1' $ C.text $ show (trunc (100.0 * Geo.distance fv.coords hh.latLng) / 100.0) <> " mi"
  , M.tcell1' $ weekDisplay schedule.days
  , M.tcell1' $ C.text schedule.time
  , M.tcell1' $ C.text schedule.scheduleDescription
  ]


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
