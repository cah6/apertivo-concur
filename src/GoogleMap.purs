module GoogleMap where


import Custom.Prelude

import Concur as C
import Foreign.GoogleMaps (googleMap, infoWindow, loadScript, marker)
import Types (HappyHour)


mkGoogleMap :: forall a. Array HappyHour -> C.Widget C.HTML a
mkGoogleMap xs = do
  click <- loadScript
    [ C.unsafeMkProp "googleMapsApiKey" "AIzaSyDZh_dyyl7PJCe-haE_hGOOP7NJCnqdy4k"
    , C.unsafeMkProp "id" "script-loader"
    ]
    [ googleMap
      [ C.unsafeMkProp "id" "example-map"
      , C.unsafeMkProp "zoom" 13
      , C.unsafeMkProp "center" {lat: 42.35, lng: -83.04}
      , C.unsafeMkProp "mapContainerStyle" {height: "100%", width: "100%" }
      ]
      (map (markerWithWindow false) xs)
    ]
  liftEffect $ log $ "Click called, reloading map"
  mkGoogleMap xs


markerWithWindow :: forall a. Boolean -> HappyHour -> C.Widget C.HTML a
markerWithWindow isEnabled hh = do
  let lat = hh.latLng.latitude
      lng = hh.latLng.longitude
  _ <- if not isEnabled
    then marker
      [ C.unsafeMkProp "position" {lat: lat, lng: lng}
      , C.onClick
      ]
      [ ]
    else infoWindow
      [ C.unsafeMkProp "position" {lat: (lat + 0.004), lng: lng}
      , C.unsafeMkPropHandler "onCloseClick"
      ] [ happyHourInfoWindow hh ]
      <|> marker
      [ C.unsafeMkProp "position" {lat: lat, lng: lng}
      ] []
  liftEffect $ log $ "info window was closed!"
  markerWithWindow (not isEnabled) hh


happyHourInfoWindow :: forall a. HappyHour -> C.Widget C.HTML a
happyHourInfoWindow hh = do
  C.div []
    [ C.h1 [ C.className "title" ] [ C.text hh.restaurant ]
    , C.div [] $ mapFlipped hh.schedule $ \sch -> C.h2
        [ C.className "subtitle"]
        [ C.text sch.scheduleDescription
        ]
    ]
