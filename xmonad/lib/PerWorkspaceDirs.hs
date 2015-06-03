{-# LANGUAGE DeriveDataTypeable        #-}
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

import           Data.Maybe                  (fromMaybe)
import           XMonad.Core
import           XMonad.StackSet             (currentTag)
import qualified XMonad.Util.ExtensibleState as XS

data WorkingDirs = WD { unWD :: [(WorkspaceId, String)] } deriving Typeable

instance ExtensionClass WorkingDirs where
  initialValue = WD []

{-changeDir :: String -> WorkspaceId -> X ()-}
{-changeDir dir ws = do-}
  {-WD cdirs <- XS.get-}
  {-XS.put . WD $ (ws, dir) : filter ((/= ws) . fst) cdirs-}

changeDir :: String -> WorkspaceId -> X ()
changeDir dir ws = XS.modify $ WD . ((ws, dir) :) . filter ((/= ws) . fst) . unWD

getDir :: WorkspaceId -> X String
getDir ws = fmap (fromMaybe "~/" . lookup ws . unWD) XS.get

currentWorkspace :: X WorkspaceId
currentWorkspace = withWindowSet (return . currentTag)
