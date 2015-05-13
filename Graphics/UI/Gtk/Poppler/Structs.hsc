{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Structures for Pango
--
--  Author : Axel Simon
--
--  Created: 2 March 2008
--
--  Copyright (C) 2008 Axel Simon
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

#include <glib-2.0/glib.h>
#include <poppler.h>
#include "template-hsc-gtk2hs.h"

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
module Graphics.UI.Gtk.Poppler.Structs (
    PopplerRectangle(..),
    peekPopplerRectangle,                    
    PopplerColor(..),
    peekPopplerColor,                
    ) where
    
import Control.Monad		(liftM)
import Data.IORef
import Control.Exception

import System.Glib.FFI
import System.Glib.UTFString ( peekUTFString, UTFCorrection,
                               ofsToUTF, ofsFromUTF )
import System.Glib.GObject (makeNewGObject)

-- | Rectangles describing an area in 'Double's.
--
-- * Specifies x, y, width and height
--
data PopplerRectangle = PopplerRectangle Double Double Double Double
		      deriving Show

instance Storable PopplerRectangle where
  sizeOf _ = #{const sizeof(PopplerRectangle)}
  alignment _ = alignment (undefined:: #gtk2hs_type gdouble)
  peek ptr = do
    (PopplerRectangle x1_ y1_ x2_ y2_) <- peekPopplerRectangle ptr
    return $ PopplerRectangle (realToFrac x1_) (realToFrac y1_)
                              (realToFrac x2_) (realToFrac y2_)
  poke ptr (PopplerRectangle x1 y1 x2 y2) = do
    #{poke PopplerRectangle, x1} ptr (x1::#gtk2hs_type gdouble)
    #{poke PopplerRectangle, y1} ptr (y1::#gtk2hs_type gdouble)
    #{poke PopplerRectangle, x2} ptr (x2::#gtk2hs_type gdouble)
    #{poke PopplerRectangle, y2} ptr (y2::#gtk2hs_type gdouble)

peekPopplerRectangle :: Ptr PopplerRectangle -> IO PopplerRectangle
peekPopplerRectangle ptr = do
    (x1_ ::#gtk2hs_type gdouble)	<- #{peek PopplerRectangle, x1} ptr
    (y1_ ::#gtk2hs_type gdouble)	<- #{peek PopplerRectangle, y1} ptr
    (x2_ ::#gtk2hs_type gdouble)	<- #{peek PopplerRectangle, x2} ptr
    (y2_ ::#gtk2hs_type gdouble)	<- #{peek PopplerRectangle, y2} ptr
    return (PopplerRectangle (realToFrac x1_) (realToFrac y1_)
                             (realToFrac x2_) (realToFrac y2_))
      
data PopplerColor = PopplerColor (#gtk2hs_type guint16) (#gtk2hs_type guint16) (#gtk2hs_type guint16)
             deriving (Eq,Show)
      
instance Storable PopplerColor where
  sizeOf _ = #{const sizeof(PopplerColor)}
  alignment _ = alignment (undefined::#gtk2hs_type guint16)
  peek ptr = peekPopplerColor ptr
  poke ptr (PopplerColor red green blue) = do
    #{poke PopplerColor, red}   ptr red
    #{poke PopplerColor, green} ptr green
    #{poke PopplerColor, blue}  ptr blue
     
peekPopplerColor :: Ptr PopplerColor -> IO PopplerColor
peekPopplerColor ptr = do
    red	   <- #{peek PopplerColor, red} ptr
    green  <- #{peek PopplerColor, green} ptr
    blue   <- #{peek PopplerColor, blue} ptr
    return $ PopplerColor red green blue