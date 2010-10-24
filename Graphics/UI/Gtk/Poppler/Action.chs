{-# LANGUAGE CPP, DeriveDataTypeable #-}
--  GIMP Toolkit (GTK) Binding for Haskell: binding to poppler -*-haskell-*-
--
--  Author : Andy Stewart
--  Created: 18-Jun-2010
--
--  Copyright (c) 2010 Andy Stewart
--
--  This library is free software: you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public License
--  as published by the Free Software Foundation, either version 3 of
--  the License, or (at your option) any later version.
--  
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--  
--  You should have received a copy of the GNU Lesser General Public
--  License along with this program.  If not, see
--  <http://www.gnu.org/licenses/>.
--  
--  POPPLER, the C library which this Haskell library depends on, is
--  available under LGPL Version 2. The documentation included with
--  this library is based on the original POPPLER documentation.
--  
-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module Graphics.UI.Gtk.Poppler.Action (
-- * Types,
    Action,
    Dest,

-- * Methods
    actionCopy,
    destCopy,
    makeNewAction,
    makeNewDest,
    ) where

import Control.Monad
import Data.Typeable
import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GList
import System.Glib.GError
import System.Glib.GObject
import System.Glib.UTFString
import Graphics.UI.Gtk.Poppler.Enums
{#import Graphics.UI.Gtk.Poppler.Types#}

{# context lib="poppler" prefix="poppler" #}

{#pointer *Action foreign newtype #}

makeNewAction :: Ptr Action -> IO Action
makeNewAction rPtr = do
  action <- newForeignPtr rPtr action_free
  return (Action action)

foreign import ccall unsafe "&poppler_action_free"
  action_free :: FinalizerPtr Action

{#pointer *Dest foreign newtype #}

makeNewDest :: Ptr Dest -> IO Dest
makeNewDest rPtr = do
  dest <- newForeignPtr rPtr dest_free
  return (Dest dest)

foreign import ccall unsafe "&poppler_dest_free"
  dest_free :: FinalizerPtr Dest

-- | Copies action, creating an identical 'Action'.
actionCopy :: Action -> IO Action
actionCopy action =
  {#call poppler_action_copy #} action
  >>= makeNewAction

-- | Copies dest, creating an identical 'Dest'.
destCopy :: Dest -> IO Dest
destCopy dest =
  {#call poppler_dest_copy #} dest
  >>= makeNewDest
