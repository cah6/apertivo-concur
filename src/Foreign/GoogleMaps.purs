module Foreign.GoogleMaps where

import Concur.Extended as C
import Control.Promise (Promise, toAff)
import Custom.Prelude
import React (ReactClass, unsafeCreateElement)
import React.DOM.Props (unsafeFromPropsArray)
import Simple.JSON as JSON

foreign import loadScriptImpl :: forall a. ReactClass a

foreign import googleMapImpl :: forall a. ReactClass a

foreign import markerImpl :: forall a. ReactClass a

foreign import infoWindowImpl :: forall a. ReactClass a

foreign import getMapBoundsPromise :: Effect (Promise Foreign)

loadScript :: C.El
loadScript = C.el' (unsafeCreateElement loadScriptImpl <<< unsafeFromPropsArray)

googleMap :: C.El
googleMap = C.el' (unsafeCreateElement googleMapImpl <<< unsafeFromPropsArray)

marker :: C.El
marker = C.el' (unsafeCreateElement markerImpl <<< unsafeFromPropsArray)

infoWindow :: C.El
infoWindow = C.el' (unsafeCreateElement infoWindowImpl <<< unsafeFromPropsArray)

getMapBounds :: Aff String
getMapBounds = do
  fgn <- liftEffect getMapBoundsPromise >>= toAff
  case JSON.read_ fgn of
    Nothing -> do
      -- liftEffect $ log "Could not get coordinates, returning 0,0"
      pure $ ""
    Just co -> pure co
