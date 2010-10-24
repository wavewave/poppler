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
module Graphics.UI.Gtk.Poppler.Page (
-- * Types
    Cairo,
    PopplerRectangle (..),
    PopplerColor (..),
    ImageMapping,
    PageTransition,
    LinkMapping,
    FormFieldMapping,

-- * Enums
    SelectionStyle (..),

-- * Methods
    pageRender,
    pageRenderToPixbuf,
    pageGetSize,
    pageGetIndex,
    pageGetThumbnail,
    pageGetThumbnailSize,
    pageRenderToPs,
    pageFindText,
    pageGetText,
    pageGetDuration,
    pageGetTransition,
    pageGetLinkMapping,
    pageGetImageMapping,
    pageGetFormFieldMapping,
    pageGetSelectionRegion,
    pageRenderSelection,
    pageRenderSelectionToPixbuf,
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
import Control.Monad.Reader (ReaderT(runReaderT), ask, MonadIO, liftIO)
import Graphics.Rendering.Cairo.Internal (Render(..), bracketR)

{# context lib="poppler" prefix="poppler" #}

-- | Render the page to the given cairo context. This function is for rendering a page that will be
-- displayed. If you want to render a page that will be printed use 'pageRenderForPrinting'
-- instead
pageRender :: PageClass page => page
 -> Render ()
pageRender page = 
  ask >>= \ x -> liftIO ({#call poppler_page_render #} (toPage page) x)

-- | First scale the document to match the specified pixels per point, then render the rectangle given by
-- the upper left corner at (@srcX@, @srcY@) and @srcWidth@ and @srcHeight@. This function is for rendering
-- a page that will be displayed. If you want to render a page that will be printed use
-- 'pageRenderToPixbufForPrinting' instead
pageRenderToPixbuf :: PageClass page => page
 -> Rectangle -- ^ @rect@      rectangle to render
 -> Double -- ^ @scale@      scale specified as pixels per point         
 -> Int -- ^ @rotation@   rotate the document by the specified degree 
 -> Pixbuf -- ^ @pixbuf@     pixbuf to render into                       
 -> IO ()
pageRenderToPixbuf page (Rectangle x y width height) scale rotation pixbuf =
  {#call poppler_page_render_to_pixbuf #}
    (toPage page)
    (fromIntegral x)
    (fromIntegral y)
    (fromIntegral width)
    (fromIntegral height)
    (realToFrac scale)
    (fromIntegral rotation)
    pixbuf

-- | Gets the size of page at the current scale and rotation.
pageGetSize :: PageClass page => page 
 -> IO (Double, Double)
pageGetSize page =
  alloca $ \ widthPtr -> 
  alloca $ \ heightPtr -> do
      {#call poppler_page_get_size #}
        (toPage page)
        widthPtr
        heightPtr
      width <- peek widthPtr
      height <- peek heightPtr
      return (realToFrac width, realToFrac height)
  
-- | Returns the index of page
pageGetIndex :: PageClass page => page
 -> IO Int  -- ^ returns index value of page 
pageGetIndex page =
  liftM fromIntegral $ 
  {#call poppler_page_get_index #} (toPage page)
  
-- | Get the embedded thumbnail for the specified page. If the document doesn't have an embedded
-- thumbnail for the page, this function returns 'Nothing'.
pageGetThumbnail :: PageClass page => page
 -> IO (Maybe Surface) -- ^ returns the tumbnail as a @cairoSurfaceT@ or 'Nothing' if the document doesn't have a thumbnail for this page.
pageGetThumbnail page = do
  surfacePtr <- {#call poppler_page_get_thumbnail #} (toPage page)
  if surfacePtr == nullPtr
     then return Nothing
     else liftM Just (mkSurface surfacePtr)

-- | Returns 'True' if page has a thumbnail associated with it. It also fills in width and height with the
-- width and height of the thumbnail. The values of width and height are not changed if no appropriate
-- thumbnail exists.
pageGetThumbnailSize :: PageClass page => page
 -> IO (Maybe (Int, Int))
pageGetThumbnailSize page = 
  alloca $ \ widthPtr -> 
  alloca $ \ heightPtr -> do
    success <- liftM toBool $
                {#call poppler_page_get_thumbnail_size #}
                  (toPage page)
                  widthPtr
                  heightPtr
    if success 
       then do
         width  <- peek widthPtr
         height <- peek heightPtr
         return $ Just (fromIntegral width, fromIntegral height)
       else return Nothing
  
-- | Render the page on a postscript file
pageRenderToPs :: (PageClass page, PSFileClass psFile) => page -> psFile -> IO ()
pageRenderToPs page psFile = 
  {#call poppler_page_render_to_ps #} (toPage page) (toPSFile psFile)

-- | A GList of rectangles for each occurance of the text on the page. The coordinates are in PDF points.
pageFindText :: PageClass page => page
 -> String  -- ^ @text@    the text to search for (UTF-8 encoded) 
 -> IO [PopplerRectangle]
pageFindText page text = 
  withUTFString text $ \ textPtr -> do
    glistPtr <- {#call poppler_page_find_text #} (toPage page) textPtr
    list <- fromGList glistPtr
    mapM peekPopplerRectangle list
    
-- | Retrieves the contents of the specified selection as text.
pageGetText :: PageClass page => page -> SelectionStyle -> PopplerRectangle
 -> IO String  -- ^ returns selection string
pageGetText page style rect = 
  with rect $ \ rectPtr ->
  {#call poppler_page_get_text #} (toPage page) ((fromIntegral . fromEnum) style) (castPtr rectPtr)
  >>= peekUTFString
  
-- | Returns the duration of page
pageGetDuration :: PageClass page => page
 -> IO Double  -- ^ returns duration in seconds of page or -1. 
pageGetDuration page =
  liftM realToFrac $
  {#call poppler_page_get_duration #} (toPage page)

-- | Returns the transition effect of page
pageGetTransition :: PageClass page => page
 -> IO (Maybe PageTransition) -- ^ returns a 'PageTransition' or 'Nothing'. 
pageGetTransition page = do
  ptr <- {#call poppler_page_get_transition #} (toPage page)
  if ptr == nullPtr
     then return Nothing
     else liftM Just $ makeNewPageTransition (castPtr ptr)

{#pointer *PageTransition foreign newtype #}

makeNewPageTransition :: Ptr PageTransition -> IO PageTransition
makeNewPageTransition rPtr = do
  transition <- newForeignPtr rPtr page_transition_free
  return (PageTransition transition)

foreign import ccall unsafe "&poppler_page_transition_free"
  page_transition_free :: FinalizerPtr PageTransition

-- | Returns a list of 'LinkMapping' items that map from a location on page to a 'Action'. 
pageGetLinkMapping :: PageClass page => page
 -> IO [LinkMapping]
pageGetLinkMapping page = do
  glistPtr <- {#call poppler_page_get_link_mapping #} (toPage page)
  list <- fromGList glistPtr
  mappings <- mapM makeNewLinkMapping list
  {#call unsafe poppler_page_free_link_mapping #} (castPtr glistPtr)
  return mappings

{#pointer *LinkMapping foreign newtype #}

makeNewLinkMapping :: Ptr LinkMapping -> IO LinkMapping
makeNewLinkMapping rPtr = do
  linkMapping <- newForeignPtr rPtr poppler_link_mapping_free
  return (LinkMapping linkMapping)

foreign import ccall unsafe "&poppler_link_mapping_free"
  poppler_link_mapping_free :: FinalizerPtr LinkMapping

-- | Returns a list of 'ImageMapping' items that map from a location on page to a 'Action'. 
pageGetImageMapping :: PageClass page => page
 -> IO [ImageMapping]
pageGetImageMapping page = do
  glistPtr <- {#call poppler_page_get_image_mapping #} (toPage page)
  list <- fromGList glistPtr
  mappings <- mapM makeNewImageMapping list
  {#call unsafe poppler_page_free_image_mapping #} (castPtr glistPtr)
  return mappings

{#pointer *ImageMapping foreign newtype #}

makeNewImageMapping :: Ptr ImageMapping -> IO ImageMapping
makeNewImageMapping rPtr = do
  imageMapping <- newForeignPtr rPtr poppler_image_mapping_free
  return (ImageMapping imageMapping)

foreign import ccall unsafe "&poppler_image_mapping_free"
  poppler_image_mapping_free :: FinalizerPtr ImageMapping

-- | Returns a list of 'FormFieldMapping' items that map from a location on page to a 'Action'. 
pageGetFormFieldMapping :: PageClass page => page
 -> IO [FormFieldMapping]
pageGetFormFieldMapping page = do
  glistPtr <- {#call poppler_page_get_form_field_mapping #} (toPage page)
  list <- fromGList glistPtr
  mappings <- mapM makeNewFormFieldMapping list
  {#call unsafe poppler_page_free_image_mapping #} (castPtr glistPtr)
  return mappings

{#pointer *FormFieldMapping foreign newtype #}

makeNewFormFieldMapping :: Ptr FormFieldMapping -> IO FormFieldMapping
makeNewFormFieldMapping rPtr = do
  formFieldMapping <- newForeignPtr rPtr poppler_form_field_mapping_free
  return (FormFieldMapping formFieldMapping)

foreign import ccall unsafe "&poppler_form_field_mapping_free"
  poppler_form_field_mapping_free :: FinalizerPtr FormFieldMapping

-- | Returns a region containing the area that would be rendered by 'pageRenderSelection' or
-- 'pageRenderSelectionToPixbuf' as a GList of PopplerRectangle.
pageGetSelectionRegion :: PageClass page => page
 -> Double -- ^ @scale@     scale specified as pixels per point             
 -> SelectionStyle -- ^ @style@     a 'SelectionStyle'                         
 -> PopplerRectangle -- ^ @selection@ start and end point of selection as a rectangle 
 -> IO [PopplerRectangle]
pageGetSelectionRegion page scale style selection = 
  with selection $ \ selectionPtr -> do
    glistPtr <- {#call poppler_page_get_selection_region #} 
                 (toPage page)
                 (realToFrac scale)
                 ((fromIntegral . fromEnum) style)
                 (castPtr selectionPtr)
    list <- fromGList glistPtr
    rectangles <- mapM peekPopplerRectangle list
    {#call unsafe poppler_page_selection_region_free #} (castPtr glistPtr)
    return rectangles

-- | Render the selection specified by selection for page to the given cairo context. The selection will
-- be rendered, using @glyphColor@ for the glyphs and @backgroundColor@ for the selection background.
-- 
-- If non-'Nothing', @oldSelection@ specifies the selection that is already rendered to cairo, in which case
-- this function will (some day) only render the changed part of the selection.
pageRenderSelection :: PageClass page => page 
 -> PopplerRectangle -- ^ @selection@        start and end point of selection as a rectangle 
 -> PopplerRectangle -- ^ @oldSelection@    previous selection                              
 -> SelectionStyle -- ^ @style@            a 'SelectionStyle'                         
 -> PopplerColor -- ^ @glyphColor@      color to use for drawing glyphs                 
 -> PopplerColor -- ^ @backgroundColor@ color to use for the selection background       
 -> Render ()
pageRenderSelection page selection oldSelection style glyphColor backgroundColor = do
  cairo <- ask
  liftIO $ 
         with selection $ \ selectionPtr -> 
         with oldSelection $ \ oldSelectionPtr -> 
         with glyphColor $ \ glyphColorPtr -> 
         with backgroundColor $ \ backgroundColorPtr -> 
              {#call poppler_page_render_selection #}
                  (toPage page)
                  cairo
                  (castPtr selectionPtr)
                  (castPtr oldSelectionPtr)
                  ((fromIntegral . fromEnum) style)
                  (castPtr glyphColorPtr)
                  (castPtr backgroundColorPtr)
        
-- | Render the selection specified by selection for page into pixbuf. The selection will be rendered at
-- scale, using @glyphColor@ for the glyphs and @backgroundColor@ for the selection background.
-- 
-- If non-'Nothing', @oldSelection@ specifies the selection that is already rendered in pixbuf, in which case
-- this function will (some day) only render the changed part of the selection.
pageRenderSelectionToPixbuf :: PageClass page => page  
 -> Double -- ^ @scale@            scale specified as pixels per point             
 -> Int -- ^ @rotation@         rotate the document by the specified degree     
 -> Pixbuf -- ^ @pixbuf@           pixbuf to render to                             
 -> PopplerRectangle -- ^ @selection@        start and end point of selection as a rectangle 
 -> PopplerRectangle -- ^ @oldSelection@    previous selection                              
 -> SelectionStyle -- ^ @style@            a 'SelectionStyle'                         
 -> Color -- ^ @glyphColor@      color to use for drawing glyphs                 
 -> Color -- ^ @backgroundColor@ color to use for the selection background       
 -> IO ()
pageRenderSelectionToPixbuf page scale rotation pixbuf selection oldSelection style glyphColor backgroundColor = 
  with selection $ \ selectionPtr -> 
  with oldSelection $ \ oldSelectionPtr -> 
  with glyphColor $ \ glyphColorPtr -> 
  with backgroundColor $ \ backgroundColorPtr -> 
      {#call poppler_page_render_selection_to_pixbuf #}
        (toPage page)
        (realToFrac scale)
        (fromIntegral rotation)
        pixbuf
        (castPtr selectionPtr)
        (castPtr oldSelectionPtr)
        ((fromIntegral . fromEnum) style)
        (castPtr glyphColorPtr)
        (castPtr backgroundColorPtr)
