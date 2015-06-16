{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module Snap.Snaplet.AcidState
  ( Acid(..)
  , HasAcid(..)
  , acidInit
  , acidInit'
  , acidInitMemory
  , acidInitManual
  , getAcidState
  , update
  , query
  , createCheckpoint
  , createArchive
  , closeAcidState
  , initWorker

  , module Data.Acid
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.State.Class
import qualified Data.Acid as A
import qualified Data.Acid.Advanced as A
import qualified Data.Acid.Memory as AM
import           Data.Acid hiding (update
                                  ,query
                                  ,createCheckpoint
                                  ,createArchive
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
newtype Acid st = Acid
    { _acidStore :: A.AcidState st
    }


------------------------------------------------------------------------------
-- | Initializer that stores the state in the \"state\/[typeOf state]\/\"
-- directory.
acidInit :: (A.IsAcidic st, Typeable st)
         => st
         -- ^ Initial state to be used if 
         -> SnapletInit b (Acid st)
acidInit initial = makeSnaplet "acid-state" description Nothing $
    initWorker (A.openLocalState initial)


------------------------------------------------------------------------------
-- | Initializer allowing you to specify the location of the acid-state store.
acidInit' :: A.IsAcidic st
          => FilePath
          -- ^ Location of the acid-state store on disk
          -> st
          -- ^ Initial state to be used if 
          -> SnapletInit b (Acid st)
acidInit' location initial = makeSnaplet "acid-state" description Nothing $
    initWorker (A.openLocalStateFrom location initial)

------------------------------------------------------------------------------
-- | Initializer allowing you to open an in-memory store (typically for testing)
acidInitMemory :: A.IsAcidic st
               => st
               -- ^ Initial state to be used if 
               -> SnapletInit b (Acid st)
acidInitMemory initial = makeSnaplet "acid-state" description Nothing $
    initWorker (AM.openMemoryState initial)


------------------------------------------------------------------------------
-- | Initializer allowing you to specify the AcidState to use.  This AcidState
-- must be initialized manually elsewhere.  It will not be automatically
-- closed by the snaplet.
acidInitManual :: A.IsAcidic st
               => AcidState st
               -- ^ AcidState state to be used
               -> SnapletInit b (Acid st)
acidInitManual initial = makeSnaplet "acid-state" description Nothing $
    return $ Acid initial


------------------------------------------------------------------------------
-- | Core init functionality common to both exported variants.
initWorker :: IO (AcidState st) -> Initializer b v (Acid st)
initWorker f = do
    st <- liftIO f
    onUnload (A.closeAcidState st)
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
class HasAcid myState acidState | myState -> acidState where
    getAcidStore :: myState -> (Acid acidState)


instance HasAcid (Acid st) st where
    getAcidStore = id


------------------------------------------------------------------------------
-- | Lower-level function providing direct access to the AcidState data type.
getAcidState :: (HasAcid s st, MonadSnaplet m, MonadState s (m b b))
    => m b v (AcidState st)
getAcidState = withTop' id $ gets $ _acidStore . getAcidStore


------------------------------------------------------------------------------
-- | Wrapper for acid-state's update function that works for arbitrary
-- instances of HasAcid.
update :: (HasAcid s (A.MethodState event),
           MonadSnaplet m,
           MonadState s (m b b),
           UpdateEvent event,
           MonadIO (m b v))
       => event -> m b v (EventResult event)
update event = do
    st <- getAcidState
    liftIO $ A.update st event


------------------------------------------------------------------------------
-- | Wrapper for acid-state's query function that works for arbitrary
-- instances of HasAcid.
query :: (HasAcid s (A.MethodState event),
          MonadSnaplet m,
          MonadState s (m b b),
          QueryEvent event,
          MonadIO (m b v))
      => event -> m b v (EventResult event)
query event = do
    st <- getAcidState
    liftIO $ A.query st event


------------------------------------------------------------------------------
-- | Wrapper for acid-state's createCheckpoint function that works for
-- arbitrary instances of HasAcid.
createCheckpoint :: (MonadIO (m b v),
                     MonadSnaplet m,
                     MonadState s (m b b),
                     HasAcid s st)
                 => m b v ()
createCheckpoint = do
    st <- getAcidState
    liftIO $ A.createCheckpoint st


------------------------------------------------------------------------------
-- | Wrapper for acid-state's createArchive function that works for
-- arbitrary instances of HasAcid.
createArchive :: (MonadIO (m b v),
                  MonadSnaplet m,
                  MonadState s (m b b),
                  HasAcid s st)
              => m b v ()
createArchive = do
    st <- getAcidState
    liftIO $ A.createArchive st


------------------------------------------------------------------------------
-- | Wrapper for acid-state's closeAcidState function that works for
-- arbitrary instances of HasAcid.  The state is automatically closed by the
-- snaplet's unload action, but this is here in case the user needs more
-- control.
closeAcidState :: (MonadIO (m b v),
                   MonadSnaplet m,
                   MonadState s (m b b),
                   HasAcid s st)
               => m b v ()
closeAcidState = do
    st <- getAcidState
    liftIO $ A.closeAcidState st

