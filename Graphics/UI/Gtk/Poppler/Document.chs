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
-- TODO :
--      poppler_font_info_scan
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
    documentNewFromData,
    documentSave,
    documentGetNPages,
    documentGetPage,
    documentGetPageByLabel,
    documentFindDest,
    documentHasAttachments,
    documentGetAttachments,
    documentGetFormField,

    indexIterNew,
    indexIterCopy,
    indexIterGetChild,
    indexIterIsOpen,
    indexIterNext,
    indexIterGetAction,

    fontInfoNew,
    fontsIterCopy,
    fontsIterGetName,
    fontsIterGetFullName,
    fontsIterGetFontType,
    fontsIterIsEmbedded,
    fontsIterIsSubset,
    fontsIterNext,

    psFileNew,
    psFileSetPaperSize,
    psFileSetDuplex,

-- * Attributes
    documentAuthor,
    documentCreationDate,
    documentCreator,
    documentFormat,
    documentFormatMajor,
    documentFormatMinor,
    documentKeywords,
    documentLinearized,
    documentMetadata,
    documentModDate,
    documentPageLayout,
    documentPageMode,
    documentPermissions,
    documentProducer,
    documentSubject,
    documentTitle,
    documentViewerPreferences,
    documentLabel,
    ) where

import Control.Monad
import Data.Typeable
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GList
import System.Glib.GError
import System.Glib.GObject
import System.Glib.UTFString
import Graphics.UI.Gtk.Poppler.Enums
{#import Graphics.UI.Gtk.Poppler.Types#}

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
  maybeWith withUTFString password $ \ passwordPtr -> 
      propagateGError ({# call poppler_document_new_from_file #} 
                       uriPtr
                       passwordPtr)

-- | Creates a new PopplerDocument. If 'Nothing' is returned, then error will be set. Possible errors include
-- those in the PopplerError and GFileError domains.
documentNewFromData :: 
   String  -- ^ @data@     the pdf data contained in a char array    
 -> Maybe String  -- ^ @password@ password to unlock the file with, or 'Nothing' 
 -> IO (Maybe Document) -- ^ returns  A newly created PopplerDocument, or 'Nothing'  
documentNewFromData dat password = 
  maybeNull (makeNewGObject mkDocument) $
  withUTFString dat $ \ datPtr -> 
  maybeWith withUTFString password $ \ passwordPtr -> 
      propagateGError ({#call poppler_document_new_from_data #}
                       datPtr
                       (fromIntegral (length dat))
                       passwordPtr)

-- | Saves document. Any change made in the document such as form fields filled by the user will be
-- saved. If error is set, 'False' will be returned. Possible errors include those in the GFileError
-- domain.
documentSave :: DocumentClass doc => doc 
 -> String -- ^ @uri@      uri of file to save                          
 -> IO Bool -- ^ returns  'True', if the document was successfully saved 
documentSave doc uri = 
  liftM toBool $
  withUTFString uri $ \ uriPtr -> 
      propagateGError ({#call poppler_document_save #}
                       (toDocument doc)
                       uriPtr)

-- | Returns the number of pages in a loaded document.
documentGetNPages :: DocumentClass doc => doc
 -> IO Int  -- ^ returns  Number of pages   
documentGetNPages doc = 
  liftM fromIntegral $ 
  {#call poppler_document_get_n_pages #} (toDocument doc)

-- | Returns the PopplerPage indexed at index. This object is owned by the caller.
-- | PopplerPages are indexed starting at 0.
documentGetPage :: DocumentClass doc => doc
 -> Int  -- ^ @index@    a page index             
 -> IO Page -- ^ returns  The PopplerPage at index 
documentGetPage doc index = 
  makeNewGObject mkPage $ 
  {#call poppler_document_get_page#} (toDocument doc) (fromIntegral index)
  
-- | Returns the PopplerPage reference by label. This object is owned by the caller. label is a
-- human-readable string representation of the page number, and can be document specific. Typically, it
-- is a value such as "iii" or "3".
-- 
-- By default, "1" refers to the first page.
documentGetPageByLabel :: DocumentClass doc => doc
 -> String -- ^ @label@    a page label                         
 -> IO Page -- ^ returns  The PopplerPage referenced by label 
documentGetPageByLabel doc label = 
  makeNewGObject mkPage $ 
  withUTFString label $ \ labelPtr -> 
  {#call poppler_document_get_page_by_label #} (toDocument doc) labelPtr

-- | Finds named destination @linkName@ in document
documentFindDest :: DocumentClass doc => doc 
 -> String -- ^ @linkName@ a named destination
 -> IO (Maybe Dest) -- ^ returns   The PopplerDest destination or 'Nothing' if @linkName@ is not a destination. 
documentFindDest doc linkName = 
  withUTFString linkName $ \ linkNamePtr -> do
      destPtr <- {#call poppler_document_find_dest #}
                  (toDocument doc)
                  linkNamePtr
      if destPtr == nullPtr
         then return Nothing
         else do
           dest <- makeNewGObject mkDest $ return destPtr
           {#call unsafe poppler_dest_free #} dest
           return $ Just dest
           
-- | Returns 'True' of document has any attachments.
documentHasAttachments :: DocumentClass doc => doc
 -> IO Bool -- ^ returns  'True', if document has attachments. 
documentHasAttachments doc = 
  liftM toBool $
  {#call poppler_document_has_attachments #} (toDocument doc)

-- | Returns the PopplerFormField for the given id.
documentGetFormField :: DocumentClass doc => doc
 -> Int  -- ^ @id@       an id of a PopplerFormField                 
 -> IO (Maybe FormField)
documentGetFormField doc id = 
  maybeNull (makeNewGObject mkFormField) $
  {#call poppler_document_get_form_field #} 
    (toDocument doc) 
    (fromIntegral id)

-- | Create a new postscript file to render to
psFileNew :: DocumentClass doc => doc
 -> String -- ^ @filename@   the path of the output filename 
 -> Int -- ^ @firstPage@ the first page to print         
 -> Int  -- ^ @nPages@    the number of pages to print    
 -> IO PSFile
psFileNew doc filename firstPage nPages =  
  makeNewGObject mkPSFile $ 
  withUTFString filename $ \ filenamePtr -> 
  {#call poppler_ps_file_new #}
    (toDocument doc)
    filenamePtr
    (fromIntegral firstPage)
    (fromIntegral nPages)
  
-- | Set the output paper size. These values will end up in the DocumentMedia, the BoundingBox DSC
-- comments and other places in the generated PostScript.
psFileSetPaperSize :: PSFileClass file => 
 file  -- ^ @psFile@ a PopplerPSFile which was not yet printed to. 
 -> Double -- ^ @width@   the paper width in 1/72 inch                  
 -> Double -- ^ @height@  the paper height in 1/72 inch                 
 -> IO () 
psFileSetPaperSize psFile width height = 
  {#call poppler_ps_file_set_paper_size #}
    (toPSFile psFile)
    (realToFrac width)
    (realToFrac height)

-- | Enable or disable Duplex printing.
psFileSetDuplex :: PSFileClass file => 
  file -- ^ @psFile@ a PopplerPSFile which was not yet printed to                     
 -> Bool  -- ^ @duplex@  whether to force duplex printing (on printers which support this)
 -> IO ()
psFileSetDuplex psFile duplex =
  {#call poppler_ps_file_set_duplex#}
    (toPSFile psFile)
    (fromBool duplex)

-- | Returns a GList containing PopplerAttachments.
documentGetAttachments :: DocumentClass doc => doc
 -> IO [Attachment]
documentGetAttachments doc = do
  glistPtr <- {#call poppler_document_get_attachments #} (toDocument doc)
  list <- fromGList glistPtr
  attachs <- mapM (makeNewGObject mkAttachment . return) list
  {#call unsafe g_list_free #} glistPtr
  return attachs

-- | Returns the root PopplerIndexIter for document, or 'Nothing'.
indexIterNew :: DocumentClass doc => doc -> IO (Maybe IndexIter)
indexIterNew doc =
  maybeNull (makeNewGObject mkIndexIter) $
  {#call poppler_index_iter_new #} (toDocument doc)

-- | Creates a new PopplerIndexIter as a copy of iter.
indexIterCopy :: IndexIterClass iter => iter -> IO IndexIter
indexIterCopy iter = 
  makeNewGObject mkIndexIter $
  {#call poppler_index_iter_copy #} (toIndexIter iter)

-- | Returns a newly created child of parent, or 'Nothing' if the iter has no child. See
-- 'indexIterNew' for more information on this function.
indexIterGetChild :: IndexIterClass iter => iter -> IO (Maybe IndexIter)
indexIterGetChild iter = 
  maybeNull (makeNewGObject mkIndexIter) $
  {#call poppler_index_iter_get_child #} (toIndexIter iter)

-- | Returns whether this node should be expanded by default to the user. The document can provide a hint
-- as to how the document's index should be expanded initially.
indexIterIsOpen :: IndexIterClass iter => iter
 -> IO Bool  -- ^ returns 'True', if the document wants iter to be expanded 
indexIterIsOpen iter =
  liftM toBool $
  {#call poppler_index_iter_is_open #} (toIndexIter iter)

-- | Sets iter to point to the next action at the current level, if valid. See 'indexIterNew'
-- for more information.
indexIterNext :: IndexIterClass iter => iter
 -> IO Bool -- ^ returns 'True', if iter was set to the next action 
indexIterNext iter =
  liftM toBool $
  {#call poppler_index_iter_next #} (toIndexIter iter)

-- | Returns the PopplerAction associated with iter. 
indexIterGetAction :: IndexIterClass iter => iter -> IO Action
indexIterGetAction iter =
  makeNewGObject mkAction $
  {#call poppler_index_iter_get_action #} (toIndexIter iter)

-- |
fontInfoNew :: DocumentClass doc => doc -> IO FontInfo  
fontInfoNew doc =
  makeNewGObject mkFontInfo $
  {#call poppler_font_info_new#} (toDocument doc)

-- | 
fontsIterCopy :: FontsIterClass iter => iter -> IO FontsIter
fontsIterCopy iter =
  makeNewGObject mkFontsIter $
  {#call poppler_fonts_iter_copy #} (toFontsIter iter)

-- |
fontsIterGetName :: FontsIterClass iter => iter -> IO String
fontsIterGetName iter =
  {#call poppler_fonts_iter_get_name #} (toFontsIter iter)
  >>= peekUTFString

-- |
fontsIterGetFullName :: FontsIterClass iter => iter -> IO String
fontsIterGetFullName iter =
  {#call poppler_fonts_iter_get_full_name #} (toFontsIter iter)
  >>= peekUTFString

-- |
fontsIterGetFontType :: FontsIterClass iter => iter -> IO FontType
fontsIterGetFontType iter =
  liftM (toEnum . fromIntegral) $
  {#call poppler_fonts_iter_get_font_type #} (toFontsIter iter)

-- |
fontsIterIsEmbedded :: FontsIterClass iter => iter
 -> IO Bool
fontsIterIsEmbedded iter =
  liftM toBool $
  {#call poppler_fonts_iter_is_embedded #} (toFontsIter iter)

-- |
fontsIterIsSubset :: FontsIterClass iter => iter
 -> IO Bool
fontsIterIsSubset iter =
  liftM toBool $
  {#call poppler_fonts_iter_is_subset #} (toFontsIter iter)

-- | 
fontsIterNext :: FontsIterClass iter => iter
 -> IO Bool -- ^ returns 'True', if iter was set to the next action 
fontsIterNext iter =
  liftM toBool $
  {#call poppler_fonts_iter_next #} (toFontsIter iter)

-------------------
-- Attributes
-- | The author of the document.
-- 
-- Default value: \"\"
documentAuthor :: DocumentClass doc => ReadAttr doc String 
documentAuthor = readAttrFromStringProperty "author"

-- | The date and time the document was created.
-- 
-- Allowed values: >= 0
-- 
-- Default value: 0
documentCreationDate :: DocumentClass doc => ReadAttr doc Int
documentCreationDate = readAttrFromIntProperty "creation-date"

-- | The software that created the document.
-- 
-- Default value: \"\"
documentCreator :: DocumentClass doc => ReadAttr doc String
documentCreator = readAttrFromStringProperty "creator"

-- | The PDF version of the document.
-- 
-- Default value: \"\"
documentFormat :: DocumentClass doc => ReadAttr doc String
documentFormat = readAttrFromStringProperty "format"

-- | The PDF major version number of the document.
-- 
-- Default value: 1
documentFormatMajor :: DocumentClass doc => ReadAttr doc String
documentFormatMajor = readAttrFromStringProperty "format-major"

-- | The PDF minor version number of the document.
-- 
-- Default value: 0
documentFormatMinor :: DocumentClass doc => ReadAttr doc String
documentFormatMinor = readAttrFromStringProperty "format-minor"

-- | Keywords.
-- 
-- Default value: \"\"
documentKeywords :: DocumentClass doc => ReadAttr doc String
documentKeywords = readAttrFromStringProperty "keywords"

-- | Is the document optimized for web viewing?.
-- 
-- Default value: \"\"
documentLinearized :: DocumentClass doc => ReadAttr doc String
documentLinearized = readAttrFromStringProperty "linearized"

-- | Embedded XML metadata.
-- 
-- Default value: \"\"
documentMetadata :: DocumentClass doc => ReadAttr doc String
documentMetadata = readAttrFromStringProperty "metadata"

-- | The date and time the document was modified.
-- 
-- Allowed values: >= 0
-- 
-- Default value: 0
documentModDate :: DocumentClass doc => ReadAttr doc Int
documentModDate = readAttrFromIntProperty "mod-date"

-- | Initial Page Layout.
-- 
-- Default value: PopplerPageLayoutUnset
documentPageLayout :: DocumentClass doc => ReadAttr doc PageLayout
documentPageLayout = readAttrFromEnumProperty "page-layout"
                     {#call pure unsafe poppler_page_layout_get_type #}

-- | Page Mode.
-- 
-- Default value: PopplerPageModeUnset
documentPageMode :: DocumentClass doc => ReadAttr doc PageMode
documentPageMode = readAttrFromEnumProperty "page-mode"
                   {#call pure unsafe poppler_page_mode_get_type #}

-- | Permissions.
-- 
-- Default value: 'PermissionsFull'
documentPermissions :: DocumentClass doc => ReadAttr doc Permissions
documentPermissions = readAttrFromEnumProperty "permissions"
                      {#call pure unsafe poppler_permissions_get_type #}

-- | The software that converted the document.
-- 
-- Default value: \"\"
documentProducer :: DocumentClass doc => ReadAttr doc String
documentProducer = readAttrFromStringProperty "producer"

-- | Subjects the document touches.
-- 
-- Default value: \"\"
documentSubject :: DocumentClass doc => ReadAttr doc String
documentSubject = readAttrFromStringProperty "subject"

-- | The title of the document.
-- 
-- Default value: \"\"
documentTitle :: DocumentClass doc => ReadAttr doc String
documentTitle = readAttrFromStringProperty "title"

-- | Viewer Preferences.
documentViewerPreferences :: DocumentClass doc => ReadAttr doc ViewerPreferences
documentViewerPreferences = readAttrFromEnumProperty "viewer-preferences"
                            {#call pure unsafe poppler_viewer_preferences_get_type #}

-- | The label of the page.
-- 
-- Default value: \"\"
documentLabel :: DocumentClass doc => ReadAttr doc String
documentLabel = readAttrFromStringProperty "label"
