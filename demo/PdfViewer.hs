-- | PDF viewer demo
--  Author      :  Andy Stewart
--  Copyright   :  (c) 2010 Andy Stewart <lazycat.manatee@gmail.com>

-- | The PDF viewer base on poppler library.
--
-- Usage:
--      pdfviewer file
--

module Main where

import Control.Applicative
import Control.Concurrent.STM
import Data.Maybe
import Control.Monad
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Poppler.Document
import System.Process
import System.Environment 

data Viewer =
    Viewer {viewerArea          :: DrawingArea
           ,viewerSurface       :: Surface
           ,viewerDocument      :: Document
           ,viewerPage          :: TVar Int}

-- | Main entry.
main :: IO ()
main = do
  -- Get program arguments.
  args <- getArgs
  case args of
    -- Display help
    ["--help"] -> 
       putStrLn $ "PDF viewer demo. \n\n" ++ 
                  "Usage: pdfviewer file\n\n"
    -- Start program.
    [arg]    -> viewerMain arg
    _        -> putStrLn "Usage: pdfviewer file"

-- | Internal browser fucntion.
viewerMain :: FilePath -> IO ()
viewerMain file = do
  -- Init.
  initGUI
  
  -- Create window.
  window <- windowNew
  windowSetDefaultSize window 900 600
  windowSetPosition window WinPosCenter

  scrolledWindow <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrolledWindow PolicyAutomatic PolicyAutomatic
  
  viewer <- viewerNew file
  let area = viewerArea viewer
  scrolledWindowAddWithViewport scrolledWindow area

  -- area `on` exposeEvent $ tryEvent $ do
                    
  
  window `containerAdd` scrolledWindow

  -- Show window.
  window `onDestroy` mainQuit
  widgetShowAll window

  mainGUI

viewerNew :: FilePath -> IO Viewer
viewerNew file = 
  Viewer <$> drawingAreaNew
         <*> createImageSurface FormatRGB24 0 0
         <*> liftM (fromMaybe (error "Error when open pdf file.")) (documentNewFromFile file Nothing)
         <*> newTVarIO 0

