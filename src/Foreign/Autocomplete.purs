module Foreign.Autocomplete where

import Custom.Prelude ((<<<))
import React (ReactClass, unsafeCreateElement)
import Concur.Extended as C
import React.DOM.Props (unsafeFromPropsArray)

foreign import autocompleteImpl :: forall a. ReactClass a

autocomplete :: C.El
autocomplete = C.el' (unsafeCreateElement autocompleteImpl <<< unsafeFromPropsArray)
