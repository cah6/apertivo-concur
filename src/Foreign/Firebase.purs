module Foreign.Firebase
  ( getCollection
  , getCollectionTyped
  , signInWithPopup
  , signInWithRedirect
  ) where


import Custom.Prelude

import Types (HappyHour)
import Control.Promise (Promise)
import Control.Promise as Promise
import Effect.Exception as E
import Simple.JSON as JSON


foreign import data FHappyHour :: Type
foreign import getCollectionImpl :: Effect (Promise Foreign)

foreign import signInWithPopupImpl :: Effect (Promise Foreign)
foreign import signInWithRedirectImpl :: Effect (Promise Foreign)



getCollection :: Aff Foreign
getCollection = liftEffect getCollectionImpl >>= Promise.toAff

getCollectionTyped :: Aff (Array HappyHour)
getCollectionTyped = do
  res <- getCollection
  -- liftEffect (log ("got raw json: " <> show res))
  case JSON.read res of
    Left e -> do
      liftEffect $ log $ "got error while parsing: " <> show e
      pure []
    Right xs -> pure xs


signInWithRedirect :: Aff UserCredential
signInWithRedirect = do
  res <- liftEffect signInWithRedirectImpl
  resAff <- Promise.toAff res
  case JSON.read resAff of
    Left e -> liftEffect $ E.throw  $ "got parse error while signing in with redirect: " <> show e
    Right v -> pure v


signInWithPopup :: Aff UserCredential
signInWithPopup = do
  res <- liftEffect signInWithPopupImpl
  resAff <- Promise.toAff res
  case JSON.read resAff of
    Left e -> liftEffect $ E.throw  $ "got parse error while signing in with popup: " <> show e
    Right v -> pure v


type UserCredential =
  { user :: User
  }

type User =
  { displayName :: String
  , email :: String
  }
