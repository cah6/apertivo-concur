module Foreign.Extras where

import Custom.Prelude

import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), to)
import React.DOM.Dynamic (a)
import Simple.JSON as JSON


------------------------------------------------------------------------------
enumReadForeign :: forall a rep
   . Generic a rep
  => EnumReadForeign rep
  => Foreign
  -> F a
enumReadForeign f =
  to <$> enumReadForeignImpl f

class EnumReadForeign rep where
  enumReadForeignImpl :: Foreign -> F rep

instance sumEnumReadForeign ::
  ( EnumReadForeign a
  , EnumReadForeign b
  ) => EnumReadForeign (Sum a b) where
  enumReadForeignImpl f
      = Inl <$> enumReadForeignImpl f
    <|> Inr <$> enumReadForeignImpl f

instance constructorEnumReadForeign ::
  ( IsSymbol name
  ) => EnumReadForeign (Constructor name NoArguments) where
  enumReadForeignImpl f = do
    s <- JSON.readImpl f
    if s == name
       then pure $ Constructor NoArguments
       else throwError <<< pure <<< ForeignError $
            "Enum string " <> s <> " did not match expected string " <> name
    where
      name = reflectSymbol (SProxy :: SProxy name)
