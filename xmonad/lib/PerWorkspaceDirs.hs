{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ImplicitParams            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -O2 #-}
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

changeDir :: String -> WorkspaceId -> X ()
changeDir dir ws = do
  WD cdirs <- XS.get
  XS.put . WD $ (ws, dir) : filter ((/= ws) . fst) cdirs

changeDir' :: String -> WorkspaceId -> X ()
changeDir' dir ws = XS.modify $ WD . ((ws, dir) :) . filter ((/= ws) . fst) . unWD

getDir :: WorkspaceId -> X String
getDir ws = do
  WD dirs <- XS.get
  return $ fromMaybe "~/" (lookup ws dirs)

currentWorkspace :: X WorkspaceId
currentWorkspace = withWindowSet (return . currentTag)
