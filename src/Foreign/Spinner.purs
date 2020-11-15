module Foreign.Spinner where

import Custom.Prelude ((<<<))
import React (ReactClass, unsafeCreateElement)
import Concur.Extended as C
import React.DOM.Props (unsafeFromPropsArray)


foreign import reactSpinner :: forall a. ReactClass a


spinner :: C.El
spinner = C.el' (unsafeCreateElement reactSpinner <<< unsafeFromPropsArray)
