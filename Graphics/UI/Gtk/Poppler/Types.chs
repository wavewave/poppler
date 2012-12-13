{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
-- -*-haskell-*-
-- -------------------- automatically generated file - do not edit ----------
--  Object hierarchy for the GIMP Toolkit (GTK) Binding for Haskell
--
--  Author : Axel Simon
--
--  Copyright (C) 2001-2005 Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- #hide

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- This file reflects the Gtk+ object hierarchy in terms of Haskell classes.
--
-- Note: the mk... functions were originally meant to simply be an alias
-- for the constructor. However, in order to communicate the destructor
-- of an object to objectNew, the mk... functions are now a tuple containing
-- Haskell constructor and the destructor function pointer. This hack avoids
-- changing all modules that simply pass mk... to objectNew.
--
module Graphics.UI.Gtk.Poppler.Types (

  Document(Document), DocumentClass,
  toDocument, 
  mkDocument, unDocument,
  castToDocument, gTypeDocument,
  FontsIter(FontsIter), FontsIterClass,
  toFontsIter, 
  mkFontsIter, unFontsIter,
  castToFontsIter, gTypeFontsIter,
  Page(Page), PageClass,
  toPage, 
  mkPage, unPage,
  castToPage, gTypePage,
  FormField(FormField), FormFieldClass,
  toFormField, 
  mkFormField, unFormField,
  castToFormField, gTypeFormField,
  PSFile(PSFile), PSFileClass,
  toPSFile, 
  mkPSFile, unPSFile,
  castToPSFile, gTypePSFile,
  FontInfo(FontInfo), FontInfoClass,
  toFontInfo, 
  mkFontInfo, unFontInfo,
  castToFontInfo, gTypeFontInfo,
  Attachment(Attachment), AttachmentClass,
  toAttachment, 
  mkAttachment, unAttachment,
  castToAttachment, gTypeAttachment,
  Layer(Layer), LayerClass,
  toLayer, 
  mkLayer, unLayer,
  castToLayer, gTypeLayer
  ) where

import Foreign.ForeignPtr (ForeignPtr, castForeignPtr, unsafeForeignPtrToPtr)
#if __GLASGOW_HASKELL__>=704
import Foreign.C.Types    (CULong(..), CUInt(..))
#else
import Foreign.C.Types    (CULong, CUInt)
#endif
import System.Glib.GType	(GType, typeInstanceIsA)
import System.Glib.GObject

{# context lib="poppler" prefix="poppler" #}

-- The usage of foreignPtrToPtr should be safe as the evaluation will only be
-- forced if the object is used afterwards
--
castTo :: (GObjectClass obj, GObjectClass obj') => GType -> String
                                                -> (obj -> obj')
castTo gtype objTypeName obj =
  case toGObject obj of
    gobj@(GObject objFPtr)
      | typeInstanceIsA ((unsafeForeignPtrToPtr.castForeignPtr) objFPtr) gtype
                  -> unsafeCastGObject gobj
      | otherwise -> error $ "Cannot cast object to " ++ objTypeName


-- ******************************************************************* Document

{#pointer *Document foreign newtype #} deriving (Eq,Ord)

mkDocument = (Document, objectUnref)
unDocument (Document o) = o

class GObjectClass o => DocumentClass o
toDocument :: DocumentClass o => o -> Document
toDocument = unsafeCastGObject . toGObject

instance DocumentClass Document
instance GObjectClass Document where
  toGObject = GObject . castForeignPtr . unDocument
  unsafeCastGObject = Document . castForeignPtr . unGObject

castToDocument :: GObjectClass obj => obj -> Document
castToDocument = castTo gTypeDocument "Document"

gTypeDocument :: GType
gTypeDocument =
  {# call fun unsafe poppler_document_get_type #}

-- ****************************************************************** FontsIter

{#pointer *FontsIter foreign newtype #} deriving (Eq,Ord)

mkFontsIter = (FontsIter, objectUnref)
unFontsIter (FontsIter o) = o

class GObjectClass o => FontsIterClass o
toFontsIter :: FontsIterClass o => o -> FontsIter
toFontsIter = unsafeCastGObject . toGObject

instance FontsIterClass FontsIter
instance GObjectClass FontsIter where
  toGObject = GObject . castForeignPtr . unFontsIter
  unsafeCastGObject = FontsIter . castForeignPtr . unGObject

castToFontsIter :: GObjectClass obj => obj -> FontsIter
castToFontsIter = castTo gTypeFontsIter "FontsIter"

gTypeFontsIter :: GType
gTypeFontsIter =
  {# call fun unsafe poppler_fonts_iter_get_type #}

-- *********************************************************************** Page

{#pointer *Page foreign newtype #} deriving (Eq,Ord)

mkPage = (Page, objectUnref)
unPage (Page o) = o

class GObjectClass o => PageClass o
toPage :: PageClass o => o -> Page
toPage = unsafeCastGObject . toGObject

instance PageClass Page
instance GObjectClass Page where
  toGObject = GObject . castForeignPtr . unPage
  unsafeCastGObject = Page . castForeignPtr . unGObject

castToPage :: GObjectClass obj => obj -> Page
castToPage = castTo gTypePage "Page"

gTypePage :: GType
gTypePage =
  {# call fun unsafe poppler_page_get_type #}

-- ****************************************************************** FormField

{#pointer *FormField foreign newtype #} deriving (Eq,Ord)

mkFormField = (FormField, objectUnref)
unFormField (FormField o) = o

class GObjectClass o => FormFieldClass o
toFormField :: FormFieldClass o => o -> FormField
toFormField = unsafeCastGObject . toGObject

instance FormFieldClass FormField
instance GObjectClass FormField where
  toGObject = GObject . castForeignPtr . unFormField
  unsafeCastGObject = FormField . castForeignPtr . unGObject

castToFormField :: GObjectClass obj => obj -> FormField
castToFormField = castTo gTypeFormField "FormField"

gTypeFormField :: GType
gTypeFormField =
  {# call fun unsafe poppler_form_field_get_type #}

-- ********************************************************************* PSFile

{#pointer *PSFile foreign newtype #} deriving (Eq,Ord)

mkPSFile = (PSFile, objectUnref)
unPSFile (PSFile o) = o

class GObjectClass o => PSFileClass o
toPSFile :: PSFileClass o => o -> PSFile
toPSFile = unsafeCastGObject . toGObject

instance PSFileClass PSFile
instance GObjectClass PSFile where
  toGObject = GObject . castForeignPtr . unPSFile
  unsafeCastGObject = PSFile . castForeignPtr . unGObject

castToPSFile :: GObjectClass obj => obj -> PSFile
castToPSFile = castTo gTypePSFile "PSFile"

gTypePSFile :: GType
gTypePSFile =
  {# call fun unsafe poppler_ps_file_get_type #}

-- ******************************************************************* FontInfo

{#pointer *FontInfo foreign newtype #} deriving (Eq,Ord)

mkFontInfo = (FontInfo, objectUnref)
unFontInfo (FontInfo o) = o

class GObjectClass o => FontInfoClass o
toFontInfo :: FontInfoClass o => o -> FontInfo
toFontInfo = unsafeCastGObject . toGObject

instance FontInfoClass FontInfo
instance GObjectClass FontInfo where
  toGObject = GObject . castForeignPtr . unFontInfo
  unsafeCastGObject = FontInfo . castForeignPtr . unGObject

castToFontInfo :: GObjectClass obj => obj -> FontInfo
castToFontInfo = castTo gTypeFontInfo "FontInfo"

gTypeFontInfo :: GType
gTypeFontInfo =
  {# call fun unsafe poppler_font_info_get_type #}

-- ***************************************************************** Attachment

{#pointer *Attachment foreign newtype #} deriving (Eq,Ord)

mkAttachment = (Attachment, objectUnref)
unAttachment (Attachment o) = o

class GObjectClass o => AttachmentClass o
toAttachment :: AttachmentClass o => o -> Attachment
toAttachment = unsafeCastGObject . toGObject

instance AttachmentClass Attachment
instance GObjectClass Attachment where
  toGObject = GObject . castForeignPtr . unAttachment
  unsafeCastGObject = Attachment . castForeignPtr . unGObject

castToAttachment :: GObjectClass obj => obj -> Attachment
castToAttachment = castTo gTypeAttachment "Attachment"

gTypeAttachment :: GType
gTypeAttachment =
  {# call fun unsafe poppler_attachment_get_type #}

-- ********************************************************************** Layer

{#pointer *Layer foreign newtype #} deriving (Eq,Ord)

mkLayer = (Layer, objectUnref)
unLayer (Layer o) = o

class GObjectClass o => LayerClass o
toLayer :: LayerClass o => o -> Layer
toLayer = unsafeCastGObject . toGObject

instance LayerClass Layer
instance GObjectClass Layer where
  toGObject = GObject . castForeignPtr . unLayer
  unsafeCastGObject = Layer . castForeignPtr . unGObject

castToLayer :: GObjectClass obj => obj -> Layer
castToLayer = castTo gTypeLayer "Layer"

gTypeLayer :: GType
gTypeLayer =
  {# call fun unsafe poppler_layer_get_type #}

