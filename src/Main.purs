module Main where

import Custom.Prelude
import Types
import Concur.Extended as C
import Data.Array (mapWithIndex)
import Foreign.Firebase as Firebase
import Foreign.Functions as F
import Foreign.Geolocation as Geo
import Foreign.Spinner as Spinner
import GoogleMap (MapInput, fillVisibleItems, mkGoogleMap)
import Layouts as L
import Option (fromRecord)

main :: Effect Unit
main = C.runWidgetInDom "root" root

root :: forall a. C.Widget C.HTML a
root = do
  coords <- liftAff Geo.getLocation <|> loadingView "Please allow location access."
  -- xs <- liftAff Firebase.getCollectionTyped
  xs <- pure staticHappyHours
  input <- liftEffect $ initMapInput xs coords
  _ <- mapView input
  C.text "We never get here"

mapView :: forall a. MapInput -> C.Widget C.HTML a
mapView input = do
  newInput <-
    C.div'
      [ mkGoogleMap input
      , C.div
          [ C.style
              { position: "absolute"
              , bottom: "0px"
              , left: "0px"
              }
          ]
          $ pure
          $ L.box_
              (fromRecord { padding: "0.5rem" })
              [ L.reel_
                  (fromRecord { itemWidth: "auto", noBar: true })
                  (mapWithIndex (mkReelItem input) input.visibleItems)
              ]
      ]
  mapView newInput

initMapInput :: Array HappyHour -> Maybe Geo.Coords -> Effect MapInput
initMapInput items coords = do
  key <- F.getGoogleApiKey
  pure
    $ fillVisibleItems
        { zoom: 13
        , center: center
        , items: sortedItems
        , visibleItems: sortedItems
        , bounds:
            { northeast:
                { latitude: center.latitude + latOffset, longitude: center.longitude + lngOffset }
            , southwest:
                { latitude: center.latitude - latOffset, longitude: center.longitude - lngOffset }
            }
        , selected: Nothing
        , userLocation: center
        , googleApiKey: key
        }
  where
  sortedItems = sortList center items

  center = fromMaybe { latitude: 42.35, longitude: -83.04 } coords

  -- apprx distance from center to edges at zoom=13
  latOffset = 0.052

  lngOffset = 0.141

mkReelItem :: forall a. MapInput -> Int -> HappyHour -> C.Widget C.HTML a
mkReelItem input index hh = do
  let
    isSelected = maybe false (\selectedId -> hh.id == selectedId) input.selected

    borderWidth = if isSelected then "var(--border-thick)" else "var(--border-thin)"

    boxId = Just $ "reel-item-" <> hh.id
  L.box_
    (fromRecord { borderWidth: borderWidth, classes: [ "reel-item" ], id: boxId })
    [ L.stack_
        (fromRecord { space: "1rem" })
        $ [ C.h2'
              [ C.text $ show (index + 1) <> ". "
              , C.a
                  (if hh.link /= "" then [ C.href hh.link ] else [])
                  [ C.text hh.restaurant ]
              ]
          ]
        <> (scheduleDisplay <$> hh.schedule)
    ]

scheduleDisplay :: forall a. Schedule -> C.Widget C.HTML a
scheduleDisplay schedule =
  C.div' $ pure
    $ L.stack_
        (fromRecord { space: "1rem" })
        [ L.cluster_
            (fromRecord { justify: "space-between" })
            [ C.ul'
                [ C.li' [ weekDisplay schedule.days ]
                , C.li' [ C.p' [ C.text schedule.time ] ]
                -- , C.li' [ C.p' [ C.text schedule.scheduleDescription ] ]
                ]
            ]
        , C.p' [ C.text schedule.scheduleDescription ]
        ]

timeDisplay :: forall a. String -> C.Widget C.HTML a
timeDisplay time = do
  C.p [] [ C.text time ]

weekDisplay :: forall a. Array Weekday -> C.Widget C.HTML a
weekDisplay daysActive = do
  L.cluster_
    (fromRecord { space: "1rem" })
    [ C.ul
        []
        (dayDisplay daysActive <$> daysOrdered)
    ]

dayDisplay :: forall a. Array Weekday -> Weekday -> C.Widget C.HTML a
dayDisplay daysActive day =
  let
    isActive = elem day daysActive

    className = if isActive then "day-active" else "day-inactive"
  in
    C.li
      [ C.className className
      ]
      [ C.text (abbreviate day) ]

-- dayDisplay :: forall a. Weekday -> Boolean -> C.Widget C.HTML a
-- dayDisplay day isOn =
--   M.button
--     (onProp <> [ C.unsafeMkProp "component" "div" ])
--     [ C.text (abbreviate day) ]
--   where
--   onProp = if isOn then [ C.color "primary" ] else []
loadingView :: forall a. String -> C.Widget C.HTML a
loadingView msg =
  L.stack_
    (fromRecord {})
    [ Spinner.spinner [] []
    , C.text msg
    ]
