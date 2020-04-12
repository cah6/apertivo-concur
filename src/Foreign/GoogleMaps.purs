module Foreign.GoogleMaps where

import Custom.Prelude ((<<<))
import React (ReactClass, unsafeCreateElement)
import Concur.React.DOM (El, el')
import React.DOM.Props (unsafeFromPropsArray)


foreign import loadScriptImpl :: forall a. ReactClass a
foreign import googleMapImpl :: forall a. ReactClass a
foreign import markerImpl :: forall a. ReactClass a
foreign import infoWindowImpl :: forall a. ReactClass a


loadScript :: El
loadScript = el' (unsafeCreateElement loadScriptImpl <<< unsafeFromPropsArray)


googleMap :: El
googleMap = el' (unsafeCreateElement googleMapImpl <<< unsafeFromPropsArray)


marker :: El
marker = el' (unsafeCreateElement markerImpl <<< unsafeFromPropsArray)


infoWindow :: El
infoWindow = el' (unsafeCreateElement infoWindowImpl <<< unsafeFromPropsArray)
