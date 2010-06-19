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
module Graphics.UI.Gtk.Poppler.Layer (
-- * Types
    Layer,
    LayerClass,

-- * Methods
    layerGetTitle,
    layerIsVisible,
    layerShow,
    layerHide,
    layerIsParent,
    layerGetRadioButtonGroupId,
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

-- | Returns the name of the layer suitable for presentation as a title in a viewer's GUI
layerGetTitle :: LayerClass layer => layer 
 -> IO String  -- ^ returns a string containing the title of the layer 
layerGetTitle layer = 
  {#call poppler_layer_get_title #} (toLayer layer)
  >>= peekUTFString

-- | Returns whether layer is visible
layerIsVisible :: LayerClass layer => layer 
 -> IO Bool  -- ^ returns 'True' if layer is visible 
layerIsVisible layer =
  liftM toBool $
  {#call poppler_layer_is_visible #} (toLayer layer)

-- | Shows layer
layerShow :: LayerClass layer => layer -> IO ()
layerShow layer =
  {#call poppler_layer_show #} (toLayer layer)
  
-- | Hides layer. If layer is the parent of other nested layers, such layers will be also hidden and will
-- be blocked until layer is shown again
layerHide :: LayerClass layer => layer -> IO ()
layerHide layer =
  {#call poppler_layer_hide #} (toLayer layer)

-- | Returns whether layer is parent of other nested layers.
layerIsParent :: LayerClass layer => layer 
 -> IO Bool  -- ^ returns 'True' if layer is parent 
layerIsParent layer =
  liftM toBool $
  {#call poppler_layer_is_parent #} (toLayer layer)

-- | Returns the numeric ID the radio button group associated with layer.
layerGetRadioButtonGroupId :: LayerClass layer => layer
 -> IO Int -- ^ returns the ID of the radio button group associated with layer, or 0 if the layer is not associated to any radio button   
layerGetRadioButtonGroupId layer =
  liftM fromIntegral $
  {#call poppler_layer_get_radio_button_group_id #} (toLayer layer)