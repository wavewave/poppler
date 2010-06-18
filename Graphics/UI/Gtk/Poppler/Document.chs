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
module Graphics.UI.Gtk.Poppler.Document (
-- * Details
--
-- | The PopplerDocument is an object used to refer to a main document.

-- * Types
    Document,

-- * Methods
    documentNewFromFile,    
    ) where

import Control.Monad
import Data.Typeable
import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GError
import System.Glib.GObject
import System.Glib.UTFString
import Graphics.UI.Gtk.Poppler.Enums
{#import Graphics.UI.Gtk.Poppler.Types#}

-- {#pointer *PopplerDocument as Document foreign newtype #} deriving (Eq,Ord)

{# context lib="poppler" prefix="poppler" #}

-- | Creates a new PopplerDocument. If 'Nothing' is returned, then error will be set. Possible errors include
-- those in the PopplerError and GFileError domains.
documentNewFromFile :: 
    String  -- ^ @uri@      uri of the file to load                   
 -> Maybe String  -- ^ @password@ password to unlock the file with, or 'Nothing' 
 -> IO (Maybe Document) -- ^ returns  A newly created PopplerDocument, or 'Nothing'  
documentNewFromFile uri password = 
  maybeNull (makeNewGObject mkDocument) $
  withUTFString uri $ \ uriPtr -> 
  maybeWith withUTFString password $ \ passwordPtr -> do
      propagateGError ({# call poppler_document_new_from_file #} 
                       uriPtr
                       passwordPtr)
