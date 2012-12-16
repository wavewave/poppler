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
module Graphics.UI.Gtk.Poppler.Attachment (
-- * Types
    AttachmentSave,
    Attachment,
    AttachmentClass,

-- * Methods
    attachmentSave,
    attachmentSaveToCallback,
    ) where

import Control.Monad
import Data.Typeable
import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GError
import System.Glib.GObject
import System.Glib.GList
import System.Glib.UTFString
import Graphics.UI.Gtk.Poppler.Enums
import Graphics.UI.Gtk.Abstract.Widget (Rectangle (..), Color (..))
{#import Graphics.UI.GtkInternals#}
{#import Graphics.Rendering.Cairo.Types#}
{#import Graphics.UI.Gtk.Poppler.Types#}
import Graphics.UI.Gtk.Poppler.Structs

{# context lib="poppler" prefix="poppler" #}

type AttachmentSave =
    String
 -> Int 
 -> Bool


{- typedef gboolean (*PopplerAttachmentSaveFunc) (const gchar  *buf,
					       gsize         count,
					       gpointer      data,
					       GError      **error); -}
-- {#pointer AttachmentSaveFunc#}

type TAttachmentSaveFunc = CString -> {#type gsize#} -> Ptr ()  -> Ptr (Ptr ()) -> IO {#type gboolean#}

type AttachmentSaveFunc = FunPtr TAttachmentSaveFunc
 
foreign import ccall "wrapper" mkAttachmentSaveFunc ::
  TAttachmentSaveFunc -> IO AttachmentSaveFunc
  -- TAttachmentSaveFunc -> IO AttachmentSaveFunc
  -- (Ptr Attachment -> CString -> {#type glong#} -> Ptr () -> IO {#type gboolean#})
  -- -> IO AttachmentSaveFunc

-- | Saves @attachment@ to a file indicated by @filename@.
-- Return 'True' if the file successfully saved
attachmentSave :: AttachmentClass attachment => attachment
 -> String -- ^ @filename@ the file name to save
 -> IO Bool
attachmentSave attachment filename = 
  liftM toBool $
  withUTFString filename $ \ filenamePtr -> 
      propagateGError ({#call poppler_attachment_save #} (toAttachment attachment) filenamePtr)




-- |
attachmentSaveToCallback :: AttachmentClass attachment => attachment
 -> AttachmentSave 
 -> IO Bool
attachmentSaveToCallback attachment fun = 
  liftM toBool $ do
    funcPtr <- mkAttachmentSaveFunc $ \s i _ _ -> do
                 str <- peekUTFString s
                 return (fromBool (fun str (fromIntegral i)))
    {#call attachment_save_to_callback #} (toAttachment attachment) funcPtr (castFunPtrToPtr funcPtr) nullPtr

