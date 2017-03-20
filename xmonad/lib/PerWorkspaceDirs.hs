{-# OPTIONS_GHC -O2                    #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ImplicitParams            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-------------------------------------------------------------------------------
-- | Allows per workspace working directories
-- ----------------------------------------------------------------------------

module PerWorkspaceDirs
  ( currentWorkspace
  , changeDir
  , getDir
  ) where

import           Data.Map                    as M
import           Data.Maybe                  (fromMaybe)
import           XMonad.Core
import           XMonad.StackSet             (currentTag)
import qualified XMonad.Util.ExtensibleState as XS

newtype WorkingDirs = WD { unWD :: M.Map WorkspaceId String }
  deriving (Read, Show, Typeable)

instance ExtensionClass WorkingDirs where
  initialValue  = WD M.empty
  extensionType = PersistentExtension

changeDir :: String -> WorkspaceId -> X ()
changeDir dir ws = XS.modify (WD . M.insert ws dir . unWD)

getDir :: WorkspaceId -> X String
getDir ws = fromMaybe "~/" . M.lookup ws . unWD <$> XS.get

currentWorkspace :: X WorkspaceId
currentWorkspace = withWindowSet (return . currentTag)
