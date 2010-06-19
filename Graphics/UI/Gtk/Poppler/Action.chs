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
    ActionClass,
    Dest,
    DestClass,

-- * Methods
    actionCopy,
    destCopy,
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

-- | Copies action, creating an identical 'Action'.
actionCopy :: ActionClass action => action -> IO Action
actionCopy action =
  makeNewGObject mkAction $
  {#call poppler_action_copy #} (toAction action)

-- | Copies dest, creating an identical 'Dest'.
destCopy :: DestClass dest => dest -> IO Dest
destCopy dest =
  makeNewGObject mkDest $
  {#call poppler_dest_copy #} (toDest dest)
