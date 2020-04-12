-- Export everything
module Concur.Extra where


import Custom.Prelude

import Concur.Reexports                                               as C
import React as R
import React.DOM.Props as R

variant :: forall t1 t2. t2 -> C.Props R.Props t1
variant = C.unsafeMkProp "variant"

type_ :: forall t3 t4. t4 -> C.Props R.Props t3
type_ = C.unsafeMkProp "type"


-- mkEl :: forall t14 t8 t9.
--   ShiftMap (C.Widget (Array ReactElement)) t9 => MultiAlternative t9 => ReactClass
--                                                                         { children :: Children
--                                                                         | t14
--                                                                         }
--                                                                       -> Array (Props Props t8) -> Array (t9 t8) -> t9 t8
mkEl reactClass = C.el' (R.unsafeCreateElement reactClass <<< R.unsafeFromPropsArray)
