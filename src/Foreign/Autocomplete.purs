module Foreign.Autocomplete where

import Custom.Prelude ((<<<))
import React (ReactClass, unsafeCreateElement)
import Concur.React.DOM (El, el')
import React.DOM.Props (unsafeFromPropsArray)


foreign import autocompleteImpl :: forall a. ReactClass a


autocomplete :: El
autocomplete = el' (unsafeCreateElement autocompleteImpl <<< unsafeFromPropsArray)
