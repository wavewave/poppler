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
import Control.Monad
import Data.Maybe
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Poppler.Document
import Graphics.UI.Gtk.Poppler.Page
import System.Environment 
import System.Process

data Viewer =
    Viewer {viewerArea          :: DrawingArea
           ,viewerDocument      :: Document
           ,viewerScrolledWindow:: ScrolledWindow
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
  windowSetDefaultSize window 600 780
  windowSetPosition window WinPosCenter

  -- Create window box.
  windowBox <- vBoxNew False 0
  window `containerAdd` windowBox

  -- Create viewer.
  viewer <- viewerNew file
  let area = viewerArea viewer
      doc  = viewerDocument viewer
      sWin = viewerScrolledWindow viewer

  -- Set title.
  title <- get doc documentTitle 
  windowSetTitle window ("PdfViewer " ++ title)

  -- Create spin button to select page.
  pages <- documentGetNPages doc -- get maximum page
  spinButton <- spinButtonNewWithRange 0 (integralToDouble pages) 1.0

  -- Redraw viewer after change value of spin button.
  afterValueSpinned spinButton $ do
    page <- spinButtonGetValue spinButton
    writeTVarIO (viewerPage viewer) (truncate page)
    widgetQueueDraw area

  -- Show.
  boxPackStart windowBox sWin PackGrow 0
  boxPackStart windowBox spinButton PackNatural 0
  window `onDestroy` mainQuit
  widgetShowAll window

  mainGUI

viewerNew :: FilePath -> IO Viewer
viewerNew file = do
  area <- drawingAreaNew
  doc  <- liftM (fromMaybe (error "Error when open pdf file.")) (documentNewFromFile ("file://" ++ file) Nothing)
  sWin <- scrolledWindowNew Nothing Nothing
  page <- newTVarIO 0

  let viewer = Viewer area doc sWin page

  scrolledWindowAddWithViewport sWin area
  scrolledWindowSetPolicy sWin PolicyAutomatic PolicyAutomatic

  area `on` exposeEvent $ tryEvent $ viewerDraw viewer                    

  return viewer

viewerDraw :: Viewer -> EventM EExpose ()
viewerDraw viewer = do
  let doc = viewerDocument viewer
      area = viewerArea viewer
  (winWidth, winHeight) <- eventWindowSize                    
  liftIO $ do
    pageNumber <- readTVarIO $ viewerPage viewer
    page       <- documentGetPage doc pageNumber
    frameWin   <- widgetGetDrawWindow area
    (docWidth, docHeight) <- pageGetSize page
    let scaleX = winWidth / docWidth
        width  = winWidth
        height = scaleX * docHeight
    widgetSetSizeRequest area (truncate width) (truncate height)

    renderWithDrawable frameWin $ do 
      setSourceRGB 1.0 1.0 1.0
      scale scaleX scaleX
      pageRender page

eventWindowSize :: EventM EExpose (Double, Double)      
eventWindowSize = do
    dr    <- eventWindow
    (w,h) <- liftIO $ drawableGetSize dr
    return $ if w * h > 1
               then (fromIntegral w, fromIntegral h)
               else (1,1)

-- | Transform Int to Doube
integralToDouble :: Integral a => a -> Double
integralToDouble v = fromIntegral v :: Double

-- | The IO version of `writeTVar`.
writeTVarIO :: TVar a -> a -> IO ()
writeTVarIO a b = atomically $ writeTVar a b
