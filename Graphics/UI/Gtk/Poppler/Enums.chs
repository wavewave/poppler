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
module Graphics.UI.Gtk.Poppler.Enums (
-- * Enums
    Error(..),
    Orientation(..),
    SelectionStyle(..),
    PageTransitionType(..),
    PageTransitionAlignment(..),
    PageTransitionDirection(..),
    Backend(..),
    PageLayout(..),
    PageMode(..),
    FontType(..),
    ViewerPreferences(..),
    Permissions(..),
    ActionType(..),
    DestType(..),
    ) where

import Control.Monad
import Data.Typeable
import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GError
import System.Glib.GObject
import System.Glib.UTFString

{# context lib="poppler" prefix="poppler" #}

{# enum PopplerError as Error {underscoreToCase} with prefix = "Poppler" deriving (Eq, Ord, Bounded, Show, Typeable) #}
{# enum PopplerOrientation as Orientation {underscoreToCase} with prefix = "Poppler" deriving (Eq, Ord, Bounded, Show, Typeable) #}
{# enum PopplerSelectionStyle as SelectionStyle {underscoreToCase} with prefix = "Poppler" deriving (Eq, Ord, Bounded, Show, Typeable) #}
{# enum PopplerPageTransitionType as PageTransitionType {underscoreToCase} with prefix = "Poppler" deriving (Eq, Ord, Bounded, Show, Typeable) #}
{# enum PopplerPageTransitionAlignment as PageTransitionAlignment {underscoreToCase} with prefix = "Poppler" deriving (Eq, Ord, Bounded, Show, Typeable) #}
{# enum PopplerPageTransitionDirection as PageTransitionDirection {underscoreToCase} with prefix = "Poppler" deriving (Eq, Ord, Bounded, Show, Typeable) #}
{# enum PopplerBackend as Backend {underscoreToCase} with prefix = "Poppler" deriving (Eq, Ord, Bounded, Show, Typeable) #}

{# enum PopplerPageLayout as PageLayout {underscoreToCase} with prefix = "Poppler" deriving (Eq, Ord, Bounded, Show, Typeable) #}
{# enum PopplerPageMode as PageMode {underscoreToCase} with prefix = "Poppler" deriving (Eq, Ord, Bounded, Show, Typeable) #}
{# enum PopplerFontType as FontType {underscoreToCase} with prefix = "Poppler" deriving (Eq, Ord, Bounded, Show, Typeable) #}
{# enum PopplerViewerPreferences as ViewerPreferences {underscoreToCase} with prefix = "Poppler" deriving (Eq, Ord, Bounded, Show, Typeable) #}
{# enum PopplerPermissions as Permissions {underscoreToCase} with prefix = "Poppler" deriving (Eq, Ord, Bounded, Show, Typeable) #}
{# enum PopplerActionType as ActionType {underscoreToCase} with prefix = "Poppler" deriving (Eq, Ord, Bounded, Show, Typeable) #}
{# enum PopplerDestType as DestType {underscoreToCase} with prefix = "Poppler" deriving (Eq, Ord, Bounded, Show, Typeable) #}
