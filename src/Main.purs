module Main where

import Control.MultiAlternative
import Control.ShiftMap
import Custom.Prelude

import Concur (p)
import Concur as C
import Control.Monad.Except (ExceptT(..), runExcept, runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..))
import Data.Array (concatMap, elem, filter, range, zip)
import Data.Enum (enumFromTo, toEnum)
import Data.Int (round)
import Data.Interval (hour, minute)
import Data.JSDate (getHours, getMinutes, getTimezoneOffset, now, readDate)
import Data.Time (Time(..))
import Effect.Now (nowTime)
import Foreign.MaterialUI as M
import MaterialUI.Styles (createMuiTheme)
import MaterialUI.Theme (Theme)
import Types (HappyHour, Weekday(..), abbreviate, daysOrdered, staticHappyHours)
import Unsafe.Coerce (unsafeCoerce)

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
root = M.pickerProvider $ M.themeProvider globalTheme $ do
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
tableView = do
  _ <- C.div [] $
    [ filterAreaMui
    , table
    ]
  tableView

type FilterValues =
  { timeOfDay :: Time
  }

filterAreaMui :: forall a. C.Widget C.HTML a
filterAreaMui = do
  res <- M.grid
    [ C.unsafeMkProp "container" true
    ]
    [ box1 [] $ M.textField [ C.label "Restaurant", C.variant "outlined" ] []
    , box1 [] $ M.textField [ C.label "Description contains", C.variant "outlined" ] []
    , box1 [ C.width "125"] (timePicker Nothing)
    , box1 [] $ daySelect Monday
    , M.box [ C.mx "auto" ] [] -- to push next to right
    , box1 [] $ M.button [ C.color "primary" ] [ C.text "Create New" ]
    ]
  filterAreaMui


timePicker :: Maybe Foreign -> C.Widget C.HTML Time
timePicker mf = do
  now <- liftEffect now
  f :: Foreign <- M.timePicker (
    [ C.label "Active during"
    , C.unsafeMkPropHandler "onChange"
    , C.unsafeMkProp "minutesStep" 5
    ] <> valueProp) $
    []
  -- try to read the timestamp, reloading widget in place if it's bad (somehow)
  case runExcept (readDate f) of
    Left e -> timePicker mf
    Right epoch -> do
      hr <- liftEffect $ getHours epoch
      min <- liftEffect $ getMinutes epoch
      case Tuple (toEnum (round hr)) (toEnum (round min)) of
        Tuple (Just hrTyped) (Just minTyped) -> do
          let returnTime = Time hrTyped minTyped bottom bottom
          liftEffect $ log $ "Selected time: " <> show returnTime
          pure returnTime
        _ -> timePicker mf
  where
  valueProp = maybe [] (\f -> [ C.unsafeMkProp "value" f ]) mf


daySelect :: forall a. Weekday -> C.Widget C.HTML a
daySelect selected = do
  newDay <- M.formControl []
    [ M.inputLabel [ id "select-label"] [ C.text "On day" ]
    , M.select
      [ value_ selected
      -- this isn't THAT unsafe, since react will give us back whatever we give it
      , unsafeCoerce <<< C.unsafeTargetValue <$> C.onChange
      , C.unsafeMkProp "labelId" "select-label"
      ] $ mapFlipped daysOrdered $ \day -> M.menuItem
          [ value_ day
          ]
          [ C.div [] [ C.text $ show day] ]
    ]
  -- liftEffect $ log $ "Selected: " <> show newDay
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


id = C.unsafeMkProp "id"

value_ = C.unsafeMkProp "value"

table :: forall a. C.Widget C.HTML a
table = box1 [ C.my 2 ] $ M.paper1 [ C.elevation 2 ] $ M.table
  [ ]
  [ M.thead []
    [ M.trow [] $ mapFlipped columns $ (\c -> M.tcell [] [ C.text c ])
    ]
  , M.tbody [] $ concatMap hhToRow staticHappyHours
  ]


hhToRow :: forall a. HappyHour -> Array (C.Widget C.HTML a)
hhToRow hh = mapFlipped hh.schedule $ \schedule -> M.trow []
  [ M.tcell1' $ M.link [ C.color "initial", C.href hh.link ] [ C.text hh.restaurant ]
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
columns = ["Restaurant", "Days", "Time", "Description"]


-- Box with sensible defaults
box1 :: forall a. Array (C.ReactProps a) -> C.Widget C.HTML a -> C.Widget C.HTML a
box1 props child = M.box ([ C.mx 1, C.my "auto" ] <> props) [child]


------------------------------------------------------------------------------
-- dayOfWeekBtns :: C.Widget C.HTML (Array Weekday)
-- dayOfWeekBtns = do
--   let days :: Array (Tuple Weekday Boolean)
--       days = zip (enumFromTo Monday Sunday) (false <$ range 1 7)
--   enabledDays <- C.andd $ map singleDayBtn days
--   -- it never returns
--   pure $ map fst $ filter (\tup -> snd tup == true) enabledDays


-- singleDayBtn :: (Tuple Weekday Boolean) -> C.Widget C.HTML (Tuple Weekday Boolean)
-- singleDayBtn (Tuple dow initial) = do
--   let onClass = "button is-rounded is-selected is-active"
--       offClass = "button is-rounded"
--       currClass = if initial then onClass else offClass
--   clicked <- C.button [ C.onClick, C.className currClass ] [ C.text (show dow) ]
--   liftEffect $ log $ "clicked! " <> show currClass
--   singleDayBtn (Tuple dow (not initial))
