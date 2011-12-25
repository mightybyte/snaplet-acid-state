{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
module Snap.Snaplet.AcidState
  ( Acid
  , HasAcid(..)
  , acidInit
  , acidInit'
  , update
  , query
  , createCheckpoint
  , closeAcidState

  , module Data.Acid
  ) where

import qualified Data.Acid as A
import qualified Data.Acid.Advanced as A
import           Data.Acid hiding (update
                                  ,query
                                  ,createCheckpoint
                                  ,closeAcidState
                                  )
import           Data.Typeable
import           Data.Text (Text)
import           Snap


------------------------------------------------------------------------------
-- | 
description :: Text
description = "Snaplet providing acid-state functionality"


------------------------------------------------------------------------------
-- | Data type holding acid-state snaplet data.
data Acid st = Acid
    { _acidStore :: A.AcidState st
    }


------------------------------------------------------------------------------
-- | Initializer that stores the state in the "state/[typeOf state]/"
-- directory.
acidInit :: (A.IsAcidic st, Typeable st)
         => st
         -- ^ Initial state to be used if 
         -> SnapletInit b (Acid st)
acidInit initial = makeSnaplet "acid-state" description Nothing $ do
    st <- liftIO $ A.openLocalState initial
    return $ Acid st


------------------------------------------------------------------------------
-- | Initializer allowing you to specify the location of the acid-state store.
acidInit' :: A.IsAcidic st
          => FilePath
          -- ^ Location of the acid-state store on disk
          -> st
          -- ^ Initial state to be used if 
          -> SnapletInit b (Acid st)
acidInit' location initial = makeSnaplet "acid-state" description Nothing $ do
    st <- liftIO $ A.openLocalStateFrom location initial
    return $ Acid st


------------------------------------------------------------------------------
-- | Type class standardizing a context that holds an AcidState.
--
-- You can minimize boilerplate in your application by adding an instance like
-- the following:
--
-- > data App = App { ... _acid :: Snaplet (Acid MyState) ... }
-- > instance HasAcid App MyState where
-- >     getAcidStore = getL (snapletValue . acid)
class HasAcid myState acidState where
    getAcidStore :: myState -> (Acid acidState)


instance HasAcid (Acid st) st where
    getAcidStore = id


getAcidState :: forall a st. HasAcid a st => a -> AcidState st
getAcidState = _acidStore . getAcidStore


------------------------------------------------------------------------------
-- | Wrapper for acid-state's update function that works for arbitrary
-- instances of HasAcid.
update :: (MonadState s m, HasAcid s (A.MethodState event),
           UpdateEvent event, MonadIO m)
       => event -> m (EventResult event)
update event = do
    st <- gets getAcidState
    liftIO $ A.update st event


------------------------------------------------------------------------------
-- | Wrapper for acid-state's query function that works for arbitrary
-- instances of HasAcid.
query :: (HasAcid s (A.MethodState event),
          MonadIO m, QueryEvent event, MonadState s m)
      => event -> m (EventResult event)
query event = do
    st <- gets getAcidState
    liftIO $ A.query st event


------------------------------------------------------------------------------
-- | Wrapper for acid-state's createCheckpoint function that works for
-- arbitrary instances of HasAcid.
createCheckpoint = do
    st <- gets getAcidState
    liftIO $ A.createCheckpoint st


------------------------------------------------------------------------------
-- | Wrapper for acid-state's closeAcidState function that works for
-- arbitrary instances of HasAcid.
closeAcidState = do
    st <- gets getAcidState
    liftIO $ A.closeAcidState st

