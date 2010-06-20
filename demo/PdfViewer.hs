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
import Graphics.UI.Gtk.Poppler.Page
import Graphics.UI.Gtk.Gdk.EventM
import System.Process
import System.Environment 

data Viewer =
    Viewer {viewerArea          :: DrawingArea
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
  
  putStrLn "Debug before"
  viewer <- viewerNew file
  putStrLn "Debug before"

  let area = viewerArea viewer
      doc  = viewerDocument viewer
  scrolledWindowAddWithViewport scrolledWindow area

  area `on` exposeEvent $ tryEvent $ do
      page <- liftIO $ documentGetPage doc 0
      (width, height) <- liftIO $ pageGetSize page
      liftIO $ widgetSetSizeRequest area (truncate width) (truncate height)
             
      frameWin <- liftIO $ widgetGetDrawWindow area
      liftIO $ renderWithDrawable frameWin $ do 
        setSourceRGB 1.0 1.0 1.0
        rectangle 0.0 0.0 width height
        fill
        pageRender page
  
  window `containerAdd` scrolledWindow

  -- Show window.
  window `onDestroy` mainQuit
  widgetShowAll window

  mainGUI

viewerNew :: FilePath -> IO Viewer
viewerNew file = 
  Viewer <$> drawingAreaNew
         <*> liftM (fromMaybe (error "Error when open pdf file.")) (documentNewFromFile ("file://" ++ file) Nothing)
         <*> newTVarIO 0
