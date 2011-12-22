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
description = "Snaplet providing acid-state"


------------------------------------------------------------------------------
-- | 
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
class MonadIO m => HasAcid m st where
    getAcidStore :: m (A.AcidState st)


instance HasAcid (Handler b (Acid st)) st where
    getAcidStore = gets _acidStore


------------------------------------------------------------------------------
-- | Wrapper for acid-state's update function that works for arbitrary
-- instances of HasAcid.
--update :: (HasAcid m st, A.UpdateEvent event) => event -> m (A.EventResult event)
update :: (A.UpdateEvent event, HasAcid m (A.MethodState event))
       => event -> m (A.EventResult event)
update event = do
    st <- getAcidStore
    liftIO $ A.update st event


------------------------------------------------------------------------------
-- | Wrapper for acid-state's query function that works for arbitrary
-- instances of HasAcid.
query :: (A.QueryEvent event, HasAcid m (A.MethodState event))
      => event -> m (A.EventResult event)
query event = do
    st <- getAcidStore
    liftIO $ A.query st event


------------------------------------------------------------------------------
-- | Wrapper for acid-state's createCheckpoint function that works for
-- arbitrary instances of HasAcid.
--createCheckpoint :: HasAcid m st => m ()
createCheckpoint = do
    st <- getAcidStore
    liftIO $ A.createCheckpoint st


------------------------------------------------------------------------------
-- | Wrapper for acid-state's closeAcidState function that works for
-- arbitrary instances of HasAcid.
--closeAcidState :: HasAcid m st => m ()
closeAcidState = do
    st <- getAcidStore
    liftIO $ A.closeAcidState st

