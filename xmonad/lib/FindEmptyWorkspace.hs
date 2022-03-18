{-# OPTIONS_GHC -O2 -Wall #-}

-- An improvement on XMonad.Actions.FindEmptyWorkspace which scans the set of
-- available workspaces in a user definable order, rather than as they appear in
-- the StackSet.
module FindEmptyWorkspace
  ( findEmptyWorkspace
  , findEmptyWorkspace'
  , viewEmptyWorkspace
  , tagToEmptyWorkspace
  , sendToEmptyWorkspace
  ) where

import           Data.List
import           Data.Maybe                   (isNothing)
import           XMonad.Util.WorkspaceCompare

import           XMonad
import           XMonad.StackSet

type Shuffle x = [x] -> [x]

findEmptyWorkspace'
  :: Shuffle (Workspace i l a)
  -> StackSet i l a s sd
  -> Maybe (Workspace i l a)
findEmptyWorkspace' wssort = find (isNothing . stack) . wssort . allWorkspaces
  where
    allWorkspaces ss = (workspace . current) ss :
                       (map workspace . visible) ss ++ hidden ss

findEmptyWorkspace :: StackSet i l a s sd -> Maybe (Workspace i l a)
findEmptyWorkspace = findEmptyWorkspace' id

withEmptyWorkspace :: (WorkspaceId -> X ()) -> X ()
withEmptyWorkspace f = do
    ws <- gets windowset
    ordering <- mkWsSort getWsCompare
    whenJust (findEmptyWorkspace' ordering ws) (f . tag)

-- | Find and view an empty workspace. Do nothing if all workspaces are
-- in use.
viewEmptyWorkspace :: X ()
viewEmptyWorkspace = withEmptyWorkspace (windows . view)

-- | Tag current window to an empty workspace and view it. Do nothing if
-- all workspaces are in use.
tagToEmptyWorkspace :: X ()
tagToEmptyWorkspace = withEmptyWorkspace $ \w -> windows $ view w . shift w

-- | Send current window to an empty workspace. Do nothing if
-- all workspaces are in use.
sendToEmptyWorkspace :: X ()
sendToEmptyWorkspace = withEmptyWorkspace $ \w -> windows $ shift w
