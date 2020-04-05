module Foreign.Spinner where

import Custom.Prelude ((<<<))
import React (ReactClass, unsafeCreateElement)
import Concur.React.DOM (El, el')
import React.DOM.Props (unsafeFromPropsArray)


foreign import reactSpinner :: forall a. ReactClass a


spinner :: El
spinner = el' (unsafeCreateElement reactSpinner <<< unsafeFromPropsArray)
