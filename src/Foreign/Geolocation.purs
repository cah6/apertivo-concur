module Foreign.Geolocation where

import Custom.Prelude

import Concur.React.DOM (El, el')
import Control.Promise (Promise, toAff)
import Math as M
import React (ReactClass, unsafeCreateElement)
import React.DOM.Props (unsafeFromPropsArray)
import Simple.JSON as JSON


foreign import getLocationPromise :: forall a. Effect (Promise Foreign)


getLocation :: Aff Coords
getLocation = do
  fgn <- liftEffect getLocationPromise >>= toAff
  case JSON.read_ fgn of
    Nothing -> do
      liftEffect $ log "Could not get coordinates, returning 0,0"
      pure $ { latitude: 0.0, longitude: 0.0 }
    Just co -> pure co


-- | Results from getLocation we care about. Must match JSON returned exactly.
type Coords =
  { latitude :: Number
  , longitude :: Number
  -- , speed :: Maybe Number
  -- , heading :: ???
  }

distance :: Coords -> Coords -> Number
distance coord1 coord2 = 2.0 * r * M.asin sqrtTerm
  where
  {latitude: lat1deg, longitude: long1deg} = coord1
  {latitude: lat2deg, longitude: long2deg} = coord2

  lat1 = toRad lat1deg
  lat2 = toRad lat2deg
  long1 = toRad long1deg
  long2 = toRad long2deg

  toRad deg = deg * M.pi / 180.0

  latDiff = lat2 - lat1
  longDiff = long2 - long1

  r = 3958.8

  hav diff = M.pow (M.sin (diff / 2.0)) 2.0

  sqrtTerm = M.sqrt (hav latDiff + (M.cos lat1 * M.cos lat2 * hav longDiff))
