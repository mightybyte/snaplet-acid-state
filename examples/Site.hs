{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where
------------------------------------------------------------------------------
-- explicit imports
------------------------------------------------------------------------------

import           Prelude hiding ((.), id)
import           Control.Category ((.))
import           Control.Monad.Reader (asks)
import           Data.ByteString (ByteString)
import           Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Snap.Util.FileServe (serveDirectory)
import Snap (SnapletInit, Snaplet, Handler, 
       addRoutes, nestSnaplet, serveSnaplet,
       defaultConfig, makeSnaplet, 
       snapletValue, writeText, 
       makeLens, getL, modL, modify)
import Snap.Snaplet.AcidState (Update, Query, Acid, HasAcid (getAcidStore), 
       makeAcidic, update, query, acidInit)


------------------------------------------------------------------------------
-- acid-state code
------------------------------------------------------------------------------

data PersistentState = PersistentState
    { _psCounter :: Int
    } deriving (Show,Ord,Eq,Typeable)

makeLens ''PersistentState

deriveSafeCopy 0 'base ''PersistentState

incCounter :: Update PersistentState ()
incCounter = modify (modL psCounter (+1))
    
myQuery :: Query PersistentState Int
myQuery = asks _psCounter

makeAcidic ''PersistentState ['incCounter, 'myQuery]


------------------------------------------------------------------------------
-- snap code
------------------------------------------------------------------------------

data App = App
    { _acid :: Snaplet (Acid PersistentState)
    }

type AppHandler = Handler App App

makeLens ''App


------------------------------------------------------------------------------
-- | This instance is optional.  It just allows you to avoid putting the call
-- "with acid" in front of your calls to query and update.
instance HasAcid App PersistentState where
    getAcidStore = getL (snapletValue . acid)


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("", serveDirectory "resources/static")
         , ("/inc", update IncCounter)
         , ("/count", writeText . T.pack . show =<< query MyQuery)
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    a <- nestSnaplet "acid" acid $ acidInit (PersistentState 0)
    addRoutes routes
    return $ App a


main = serveSnaplet defaultConfig app
