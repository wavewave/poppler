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
module Graphics.UI.Gtk.Poppler.FormField (
-- * Methods
    formFieldGetId,
    formFieldGetFieldType,
    formFieldIsReadOnly,
    formFieldGetFontSize,
    formFieldButtonGetButtonType,
    formFieldButtonGetState,
    formFieldButtonSetState,
    formFieldChoiceCanSelectMultiple,
    formFieldChoiceCommitOnChange,
    formFieldChoiceDoSpellCheck,
    formFieldChoiceGetChoiceType,
    formFieldChoiceGetItem,
    formFieldChoiceGetNItems,
    formFieldChoiceGetText,
    formFieldIsEditable,
    formFieldChoiceIsItemSelected,
    formFieldChoiceSelectItem,
    formFieldChoiceSetText,
    formFieldChoiceToggleItem,
    formFieldChoiceUnselectAll,
    formFieldTextDoScroll,
    formFieldTextDoSpellCheck,
    formFieldTextGetMaxLen,
    formFieldTextGetText,
    formFieldTextGetTextType,
    formFieldTextIsPassword,
    formFieldTextIsRichText,
    formFieldTextSetText,
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

-- | Gets the id of field
formFieldGetId :: FormFieldClass field => field 
 -> IO Int  -- ^ returns the id of field    
formFieldGetId field =
  liftM fromIntegral $
  {#call poppler_form_field_get_id #} (toFormField field)

-- | Gets the type of field
formFieldGetFieldType :: FormFieldClass field => field 
 -> IO FormFieldType -- ^ returns PopplerFormFieldType of field 
formFieldGetFieldType field =
  liftM (toEnum . fromIntegral) $
  {# call poppler_form_field_get_field_type #} (toFormField field)

-- | Checks whether field is read only
formFieldIsReadOnly :: FormFieldClass field => field
 -> IO Bool -- ^ returns 'True' if field is read only 
formFieldIsReadOnly field =
  liftM toBool $ 
  {#call poppler_form_field_is_read_only #} (toFormField field)

-- | Gets the font size of field
formFieldGetFontSize :: FormFieldClass field => field
 -> IO Double -- ^ returns the font size of field 
formFieldGetFontSize field =
  liftM realToFrac $ 
  {#call poppler_form_field_get_font_size #} (toFormField field)

-- | Gets the button type of field
formFieldButtonGetButtonType :: FormFieldClass field => field 
 -> IO FormButtonType -- ^ returns PopplerFormButtonType of field 
formFieldButtonGetButtonType field =
  liftM (toEnum . fromIntegral) $
  {# call poppler_form_field_button_get_button_type #} (toFormField field)

-- | Queries a PopplerFormField and returns its current state. Returns 'True' if field is pressed in and
-- 'False' if it is raised.
formFieldButtonGetState :: FormFieldClass field => field
 -> IO Bool -- ^ returns current state of field 
formFieldButtonGetState field =
  liftM toBool $
  {#call poppler_form_field_button_get_state #} (toFormField field)

-- | Sets the status of field. Set to 'True' if you want the PopplerFormField to be 'pressed in', and 'False'
-- to raise it.
formFieldButtonSetState :: FormFieldClass field => field
 -> Bool 
 -> IO ()
formFieldButtonSetState field state =
  {#call poppler_form_field_button_set_state #} (toFormField field) (fromBool state)

-- | Checks whether field allows multiple choices to be selected
formFieldChoiceCanSelectMultiple :: FormFieldClass field => field
 -> IO Bool -- ^ returns 'True' if field allows multiple choices to be selected 
formFieldChoiceCanSelectMultiple field =
  liftM toBool $
  {#call poppler_form_field_choice_can_select_multiple#} (toFormField field)

-- |
formFieldChoiceCommitOnChange :: FormFieldClass field => field
 -> IO Bool
formFieldChoiceCommitOnChange field =
  liftM toBool $
  {#call poppler_form_field_choice_commit_on_change #} (toFormField field)

-- | Checks whether spell checking should be done for the contents of field
formFieldChoiceDoSpellCheck :: FormFieldClass field => field
 -> IO Bool -- ^ returns 'True' if spell checking should be done for field 
formFieldChoiceDoSpellCheck field =
  liftM toBool $
  {#call poppler_form_field_choice_do_spell_check #} (toFormField field)

-- | Gets the choice type of field
formFieldChoiceGetChoiceType :: FormFieldClass field => field
 -> IO FormChoiceType -- ^ returns PopplerFormChoiceType of field 
formFieldChoiceGetChoiceType field =
  liftM (toEnum . fromIntegral) $
  {#call poppler_form_field_choice_get_choice_type #} (toFormField field)

-- | Returns the contents of the item on field at the given index
formFieldChoiceGetItem :: FormFieldClass field => field 
 -> Int  -- ^ @index@   the index of the item                                            
 -> IO String
formFieldChoiceGetItem field index =
  {#call poppler_form_field_choice_get_item #} (toFormField field) (fromIntegral index)
  >>= readUTFString

-- | Returns the number of items on field
formFieldChoiceGetNItems :: FormFieldClass field => field
 -> IO Int -- ^ returns the number of items on field 
formFieldChoiceGetNItems field =
  liftM fromIntegral $
  {#call poppler_form_field_choice_get_n_items #} (toFormField field)

-- | Retrieves the contents of field.
formFieldChoiceGetText :: FormFieldClass field => field
 -> IO String
formFieldChoiceGetText field =
  {#call poppler_form_field_choice_get_text #} (toFormField field)
  >>= readUTFString

-- | Checks whether field is editable
formFieldIsEditable :: FormFieldClass field => field
 -> IO Bool -- ^ returns 'True' if field is editable 
formFieldIsEditable field =
  liftM toBool $ 
  {#call poppler_form_field_choice_is_editable #} (toFormField field)

-- | Checks whether the item at the given index on field is currently selected
formFieldChoiceIsItemSelected :: FormFieldClass field => field
 -> Int -- ^ @index@   the index of the item                       
 -> IO Bool  -- ^ returns 'True' if item at index is currently selected 
formFieldChoiceIsItemSelected field index =
  liftM toBool $
  {#call poppler_form_field_choice_is_item_selected #} (toFormField field) (fromIntegral index)

-- | Selects the item at the given index on field
formFieldChoiceSelectItem :: FormFieldClass field => field 
 -> Int -- ^ @index@ the index of the item 
 -> IO ()
formFieldChoiceSelectItem field index =
  {# call poppler_form_field_choice_select_item #} (toFormField field) (fromIntegral index)

-- | 
formFieldChoiceSetText :: FormFieldClass field => field -> String -> IO ()
formFieldChoiceSetText field text =
  withUTFString text $ \ textPtr -> 
      {#call poppler_form_field_choice_set_text #} (toFormField field) textPtr

-- | Toggles the item at the given index on field
formFieldChoiceToggleItem :: FormFieldClass field => field 
 -> Int -- ^ @index@ the index of the item 
 -> IO ()
formFieldChoiceToggleItem field index =
  {# call poppler_form_field_choice_toggle_item #} (toFormField field) (fromIntegral index)

-- | Unselects all the items on field
formFieldChoiceUnselectAll :: FormFieldClass field => field -> IO ()
formFieldChoiceUnselectAll field =
  {#call poppler_form_field_choice_unselect_all #} (toFormField field)

-- |
formFieldTextDoScroll :: FormFieldClass field => field -> IO Bool
formFieldTextDoScroll field =
  liftM toBool $
  {#call poppler_form_field_text_do_scroll #} (toFormField field)

-- | Checks whether spell checking should be done for the contents of field
formFieldTextDoSpellCheck :: FormFieldClass field => field
 -> IO Bool  -- ^ returns 'True' if spell checking should be done for field 
formFieldTextDoSpellCheck field =
  liftM toBool $
  {#call poppler_form_field_text_do_spell_check #} (toFormField field)

-- | Retrieves the maximum allowed length of the text in field
formFieldTextGetMaxLen :: FormFieldClass field => field 
 -> IO Int -- ^ returns the maximum allowed number of characters in field, or -1 if there is no maximum.
formFieldTextGetMaxLen field = 
  liftM fromIntegral $
  {#call poppler_form_field_text_get_max_len #} (toFormField field)

-- | Retrieves the contents of field.
formFieldTextGetText :: FormFieldClass field => field 
 -> IO String
formFieldTextGetText field =
  {#call poppler_form_field_text_get_text #} (toFormField field) 
  >>= readUTFString

-- | Gets the text type of field
formFieldTextGetTextType :: FormFieldClass field => field 
 -> IO FormTextType -- ^ returns PopplerFormTextType of field 
formFieldTextGetTextType field =
  liftM (toEnum . fromIntegral) $
  {# call poppler_form_field_text_get_text_type #} (toFormField field)

-- | Checks whether content of field is a password and it must be hidden
formFieldTextIsPassword :: FormFieldClass field => field 
 -> IO Bool -- ^ returns 'True' if the content of field is a password 
formFieldTextIsPassword field =
  liftM toBool $
  {#call poppler_form_field_text_is_password #} (toFormField field)

-- | Checks whether the contents of field are rich text
formFieldTextIsRichText :: FormFieldClass field => field 
 -> IO Bool -- ^ returns 'True' if the contents of field are rich text 
formFieldTextIsRichText field =
  liftM toBool $
  {#call poppler_form_field_text_is_rich_text #} (toFormField field)

-- | Sets the text in field to the given value, replacing the current contents.
formFieldTextSetText :: FormFieldClass field => field 
 -> String -- ^ @text@  the new text       
 -> IO ()
formFieldTextSetText field text =
  withUTFString text $ \ textPtr -> 
  {#call poppler_form_field_text_set_text #} (toFormField field) textPtr