module Foreign.Firebase
  ( getCollection
  , getCollectionTyped
  ) where


import Custom.Prelude

import Types (HappyHour)
import Control.Promise (Promise)
import Control.Promise as Promise

import Simple.JSON


foreign import data FHappyHour :: Type
foreign import getCollectionImpl :: Effect (Promise Foreign)


getCollection :: Aff Foreign
getCollection = liftEffect getCollectionImpl >>= Promise.toAff

getCollectionTyped :: Aff (Array HappyHour)
getCollectionTyped = do
  res <- getCollection
  -- liftEffect (log ("got raw json: " <> show res))
  case read res of
    Left e -> do
      liftEffect $ log $ "got error while parsing: " <> show e
      pure []
    Right xs -> pure xs
