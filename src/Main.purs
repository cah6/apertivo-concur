module Main where

import Custom.Prelude
import Custom.Concur as C

import Data.Array (filter, range, zip)
import Data.Enum (enumFromTo)
import Foreign.Spinner (spinner)


main :: Effect Unit
main = C.runWidgetInDom "root" hello


hello :: forall a. C.Widget C.HTML a
hello = do
  _ <- C.button [ C.onClick, C.className "button" ] [ C.text "Say Hello" ]
  _ <- spinner [ C.size 300 ] []
  _ <- C.div [C.className "buttons has-addons is-centered"] [ dayOfWeekBtns ]
  C.text "Hello Sailor!"


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
  liftEffect (log ("clicked! " <> show currClass))
  singleDayBtn (Tuple dow (not initial))
