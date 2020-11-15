module GoogleMap where

import Custom.Prelude
import Concur.Extended as C
import DOM (getElementById)
import Data.Array (filter, mapWithIndex, head)
import Foreign.Functions as F
import Foreign.GoogleMaps (googleMap, loadScript, marker)
import React.Ref (createNodeRef, fromRef, getCurrentRef)
import Types (HappyHour, LatLng)
import Unsafe.Coerce (unsafeCoerce)

type LatLngBounds
  = { northeast :: LatLng
    , southwest :: LatLng
    }

type MapInput
  = { zoom :: Int
    , center :: LatLng
    , items :: Array HappyHour
    , visibleItems :: Array HappyHour
    , bounds :: LatLngBounds
    , selected :: Maybe String
    }

fillVisibleItems :: MapInput -> MapInput
fillVisibleItems input =
  input
    { visibleItems = filter (\i -> boundsContains input.bounds i.latLng) input.items
    , selected =
      case input.selected of
        Just original -> Just original
        Nothing -> case head input.visibleItems of
          Nothing -> Nothing
          Just x -> Just x.id
    }

mkGoogleMap :: MapInput -> C.Widget C.HTML MapInput
mkGoogleMap input = do
  ref <- liftEffect createNodeRef
  selected :: Maybe String <-
    loadScript
      [ C.unsafeMkProp "googleMapsApiKey" "AIzaSyDZh_dyyl7PJCe-haE_hGOOP7NJCnqdy4k"
      , C.unsafeMkProp "id" "script-loader"
      ]
      [ googleMap
          [ C.unsafeMkProp "id" "example-map"
          , C.unsafeMkProp "zoom" input.zoom
          , C.unsafeMkProp "center" { lat: input.center.latitude, lng: input.center.longitude }
          , C.unsafeMkProp "mapContainerStyle" { height: "100%", width: "100%" }
          , C.unsafeMkProp "options" { disableDefaultUI: true }
          , C.ref (fromRef ref)
          , input.selected <$ C.unsafeMkPropHandler "onZoomChanged"
          , input.selected <$ C.unsafeMkPropHandler "onDragEnd"
          , C.unsafeMkProp "position" "fixed"
          ]
          (map Just <$> (mapWithIndex (markerWithWindow input false) input.visibleItems))
      ]
  mMap <- liftEffect (getCurrentRef ref)
  -- scroll to the reel's item if needed
  _ <- case selected of
    Nothing -> pure unit
    Just selectedVal -> do
      mReelEl <- liftEffect $ getElementById ("reel-item-" <> selectedVal)
      case mReelEl of
        Nothing -> pure unit
        Just reelEl -> do
          liftEffect $ F.scrollIntoView reelEl
  case mMap of
    Nothing -> pure $ input
    -- map was moved (dragged or zoomed in or out)
    Just map_ -> do
      let
        map = unsafeCoerce map_

        bounds = map.state.map.get "bounds"

        northeast = bounds.getNorthEast unit

        southwest = bounds.getSouthWest unit
      pure
        $ fillVisibleItems
        $ input
            { zoom = map.state.map.zoom
            , center =
              { latitude:
                  map.state.map.center.lat.apply unit
              , longitude:
                  map.state.map.center.lng.apply unit
              }
            , bounds =
              { northeast:
                  { latitude: northeast.lat unit
                  , longitude: northeast.lng unit
                  }
              , southwest:
                  { latitude: southwest.lat unit
                  , longitude: southwest.lng unit
                  }
              }
            , selected = selected
            }

boundsContains :: LatLngBounds -> LatLng -> Boolean
boundsContains bounds test =
  (test.longitude < bounds.northeast.longitude)
    && (test.longitude > bounds.southwest.longitude)
    && (test.latitude < bounds.northeast.latitude)
    && (test.latitude > bounds.southwest.latitude)

markerWithWindow :: MapInput -> Boolean -> Int -> HappyHour -> C.Widget C.HTML String
markerWithWindow input isEnabled index hh = do
  let
    isSelected = maybe false (\selectedId -> hh.id == selectedId) input.selected

    strokeWeight = if isSelected then 4 else 2

    lat = hh.latLng.latitude

    lng = hh.latLng.longitude
  _ <-
    marker
      [ C.unsafeMkProp "position" { lat: lat, lng: lng }
      -- makes a diamond
      , C.unsafeMkProp "icon"
          { path: "M 100,0 0,100 -100,0 0,-100 100,0 z"
          , fillColor: "white"
          , fillOpacity: 0.8
          , scale: 0.2
          , strokeColor: "black"
          , strokeWeight: strokeWeight
          }
      , C.label $ show (index + 1)
      , C.onClick
      ]
      []
  -- else
  --   infoWindow
  --     [ C.unsafeMkProp "position" { lat: (lat + 0.004), lng: lng }
  --     , C.unsafeMkPropHandler "onCloseClick"
  --     ]
  --     [ happyHourInfoWindow hh ]
  --     <|> marker
  --         [ C.unsafeMkProp "position" { lat: lat, lng: lng }
  --         ]
  --         []
  -- liftEffect $ log $ "info window was closed!"
  -- markerWithWindow input (not isEnabled) index hh
  pure hh.id

happyHourInfoWindow :: forall a. HappyHour -> C.Widget C.HTML a
happyHourInfoWindow hh = do
  C.div []
    [ C.h1 [ C.className "title" ] [ C.text hh.restaurant ]
    , C.div [] $ mapFlipped hh.schedule
        $ \sch ->
            C.h2
              [ C.className "subtitle" ]
              [ C.text sch.scheduleDescription
              ]
    ]
