-- Export everything
module Concur.Extra where


import Custom.Prelude

import Concur.Reexports as C
import React (Children, ReactClass)
import React as R
import React.DOM.Props as RP

unsafeCreateElement = R.unsafeCreateElement
unsafeFromPropsArray = RP.unsafeFromPropsArray


mkEl :: forall trash. ReactClass { children :: Children | trash } -> C.El
mkEl reactClass = C.el' (unsafeCreateElement reactClass <<< unsafeFromPropsArray)

wrapClass :: forall a. String -> Array (C.Widget C.HTML a) -> C.Widget C.HTML a
wrapClass clazz = C.div [ C.className clazz ]

wrapClass1 :: forall a. String -> C.Widget C.HTML a -> C.Widget C.HTML a
wrapClass1 clazz child = C.div [ C.className clazz ] [ child ]

------------------------------------------------------------------------------
-- New prop shorthands

variant :: forall a b. a -> C.ReactProps b
variant = C.unsafeMkProp "variant"

type_ :: forall a b. a -> C.ReactProps b
type_ = C.unsafeMkProp "type"

mx :: forall a b. a -> C.ReactProps b
mx = C.unsafeMkProp "mx"

my :: forall a b. a -> C.ReactProps b
my = C.unsafeMkProp "my"

elevation :: forall a b. a -> C.ReactProps b
elevation = C.unsafeMkProp "elevation"
