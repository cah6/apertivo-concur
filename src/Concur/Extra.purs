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
type AnyProp = forall a b. a -> C.ReactProps b

id :: AnyProp
id = C.unsafeMkProp "id"

variant :: AnyProp
variant = C.unsafeMkProp "variant"

type_ :: AnyProp
type_ = C.unsafeMkProp "type"

mx :: AnyProp
mx = C.unsafeMkProp "mx"

my :: AnyProp
my = C.unsafeMkProp "my"

elevation :: AnyProp
elevation = C.unsafeMkProp "elevation"

value_ :: AnyProp
value_ = C.unsafeMkProp "value"
