module Foreign.Functions where

import Custom.Prelude
import React.Ref (NativeNode)
import Web.DOM (Element)

foreign import toggleOverflowClass :: NativeNode -> Effect Unit

foreign import attachReelObserver :: NativeNode -> Effect Unit

foreign import log :: forall a. a -> Effect Unit

foreign import scrollIntoView :: Element -> Effect Unit
