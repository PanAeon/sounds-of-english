module Main where

import Lib

import Control.Monad.IO.Class (liftIO)

import Data.Functor (($>))
import qualified Graphics.UI.Gtk as Gtk
import           Graphics.UI.Gtk (AttrOp(..))
import qualified Graphics.UI.Gtk.General.CssProvider as CssProvider
import qualified Graphics.UI.Gtk.General.StyleContext as StyleContext
import  System.Process

data Video = Video {
               vTitle :: String,
               vPath  :: FilePath
             }

videos = [
           [  Video "ɪ" "/home/vitalii/Downloads/learningenglish/vowel_short_1.mp4"
           ,  Video "ʊ" "/home/vitalii/Downloads/learningenglish/vowel_short_2.mp4"
           ,  Video "ʌ" "/home/vitalii/Downloads/learningenglish/vowel_short_3.mp4"
           ,  Video "ɒ" "/home/vitalii/Downloads/learningenglish/vowel_short_4.mp4"
           ,  Video "ə" "/home/vitalii/Downloads/learningenglish/vowel_short_5.mp4"
           ,  Video "e" "/home/vitalii/Downloads/learningenglish/vowel_short_6.mp4"
           ,  Video "æ" "/home/vitalii/Downloads/learningenglish/vowel_short_7.mp4"
           ],
           [  Video "iː" "/home/vitalii/Downloads/learningenglish/vowel_long_1.mp4"
           ,  Video "uː" "/home/vitalii/Downloads/learningenglish/vowel_long_2.mp4"
           ,  Video "ɑː" "/home/vitalii/Downloads/learningenglish/vowel_long_3.mp4"
           ,  Video "ɔː" "/home/vitalii/Downloads/learningenglish/vowel_long_4.mp4"
           ,  Video "ɜː" "/home/vitalii/Downloads/learningenglish/vowel_long_5.mp4"
           ],
           [  Video "ɪə" "/home/vitalii/Downloads/learningenglish/vowel_dip_1.mp4"
           ,  Video "ʊə" "/home/vitalii/Downloads/learningenglish/vowel_dip_2.mp4"
           ,  Video "aɪ" "/home/vitalii/Downloads/learningenglish/vowel_dip_3.mp4"
           ,  Video "ɔɪ" "/home/vitalii/Downloads/learningenglish/vowel_dip_4.mp4"
           ,  Video "əʊ" "/home/vitalii/Downloads/learningenglish/vowel_dip_5.mp4"
           ,  Video "eə" "/home/vitalii/Downloads/learningenglish/vowel_dip_6.mp4"
           ,  Video "aʊ" "/home/vitalii/Downloads/learningenglish/vowel_dip_7.mp4"
           ,  Video "eɪ" "/home/vitalii/Downloads/learningenglish/vowel_dip_8.mp4"
           ],
           [  Video "p" "/home/vitalii/Downloads/learningenglish/con_voiceless_1.mp4"
           ,  Video "t" "/home/vitalii/Downloads/learningenglish/con_voiceless_2.mp4"
           ,  Video "tʃ" "/home/vitalii/Downloads/learningenglish/con_voiceless_3.mp4"
           ,  Video "k" "/home/vitalii/Downloads/learningenglish/con_voiceless_4.mp4"
           ,  Video "f" "/home/vitalii/Downloads/learningenglish/con_voiceless_5.mp4"
           ,  Video "θ" "/home/vitalii/Downloads/learningenglish/con_voiceless_6.mp4"
           ,  Video "s" "/home/vitalii/Downloads/learningenglish/con_voiceless_7.mp4"
           ,  Video "ʃ" "/home/vitalii/Downloads/learningenglish/con_voiceless_8.mp4"
           ],
           [  Video "b" "/home/vitalii/Downloads/learningenglish/con_voiced_1.mp4"
           ,  Video "d" "/home/vitalii/Downloads/learningenglish/con_voiced_2.mp4"
           ,  Video "dʒ" "/home/vitalii/Downloads/learningenglish/con_voiced_3.mp4"
           ,  Video "g" "/home/vitalii/Downloads/learningenglish/con_voiced_4.mp4"
           ,  Video "v" "/home/vitalii/Downloads/learningenglish/con_voiced_5.mp4"
           ,  Video "ð" "/home/vitalii/Downloads/learningenglish/con_voiced_6.mp4"
           ,  Video "z" "/home/vitalii/Downloads/learningenglish/con_voiced_7.mp4"
           ,  Video "ʒ" "/home/vitalii/Downloads/learningenglish/con_voiced_8.mp4"
           ],
           [  Video "m" "/home/vitalii/Downloads/learningenglish/con_other_1.mp4"
           ,  Video "n" "/home/vitalii/Downloads/learningenglish/con_other_2.mp4"
           ,  Video "ŋ" "/home/vitalii/Downloads/learningenglish/con_other_3.mp4"
           ,  Video "h" "/home/vitalii/Downloads/learningenglish/con_other_4.mp4"
           ,  Video "l" "/home/vitalii/Downloads/learningenglish/con_other_5.mp4"
           ,  Video "r" "/home/vitalii/Downloads/learningenglish/con_other_6.mp4"
           ,  Video "w" "/home/vitalii/Downloads/learningenglish/con_other_7.mp4"
           ,  Video "j" "/home/vitalii/Downloads/learningenglish/con_other_8.mp4"
           ]
         -- , []
         ]

mkAndAttachButton :: Gtk.Grid -> (Int, Int) -> Video -> IO ()
mkAndAttachButton grid (row, col) (Video title path) =
  do
    btn <- Gtk.buttonNew
    styleCtx <- Gtk.widgetGetStyleContext btn
    StyleContext.styleContextAddClass styleCtx "my-button"
    -- context = gtk_widget_get_style_context (button);
  -- gtk_style_context_add_class (context, "image-button");
    Gtk.set btn [Gtk.buttonLabel := title]
    Gtk.on btn Gtk.buttonActivated $
      do
        r <- createProcess (proc "mplayer" ["-fs", "-vo", "gl2", path])
        pure ()
    Gtk.gridAttach grid btn row col 1 1


maxCols = 8
maxRows = 6

makeGrid :: Gtk.Grid -> IO ()
makeGrid grid = sequence_ [ mkAndAttachButton grid (row,col) video |
                  (col, c) <- zip [0..] videos,
                  (row, video) <- zip [1..] c
                ]

makeLabel :: String -> IO Gtk.Label
makeLabel text = do
  label <- Gtk.labelNew (Just text)
  Gtk.miscSetPadding label 15 4
  Gtk.miscSetAlignment label 0.0 0.5
  pure label

attachLabels :: Gtk.Grid -> IO ()
attachLabels grid = sequence_ [ attachLabel i text | (i,text) <- zip [0..] labels]
   where
     labels = ["Short Vowels", "Long Vowels", "Diphthongs", "Voiceless consonants", "Voiced consonants", "Other consonants"]
     attachLabel i text = do {l <- makeLabel text; Gtk.gridAttach grid l 0 i 1 1}

createAndAttachCssProvider :: IO ()
createAndAttachCssProvider = do
  provider <- CssProvider.cssProviderNew
  CssProvider.cssProviderLoadFromString provider "*{font-family:'DejaVu Sans';} .my-button{border-radius: 5px;min-width: 2.5rem; min-height:2.5rem;}"
  screen <- maybe (error "No screen found!")  id <$> Gtk.screenGetDefault
  StyleContext.styleContextAddProviderForScreen screen provider 400
  -- Gtk.stylecon
  pure ()

main :: IO ()
main = do
    Gtk.initGUI
    window <- Gtk.windowNew

    vbox   <- Gtk.vBoxNew False 30
    grid   <- Gtk.gridNew
    createAndAttachCssProvider
    Gtk.gridSetRowHomogeneous grid True
    -- Gtk.gridSetColumnHomogeneous grid True
    Gtk.gridSetColumnSpacing grid 10
    Gtk.gridSetRowSpacing grid 10
    header <- makeLabel "Sounds of English"
    Gtk.labelSetMarkup header "<span font=\"DejaVu Sans Light 26\" weight=\"light\">Sounds of English</span>" -- TODO: Helvetica Neue
    Gtk.miscSetAlignment header 0.0 0.5
    Gtk.miscSetPadding header 20 10
    makeGrid grid
    attachLabels grid
    Gtk.boxPackStart vbox header Gtk.PackNatural 10
    Gtk.boxPackStart vbox grid Gtk.PackNatural 10
    -- button <- Gtk.buttonNew
    Gtk.set window [ Gtk.containerBorderWidth := 30
                   , Gtk.windowResizable     := True
                   , Gtk.windowDefaultWidth  := 700
                   , Gtk.windowDefaultHeight := 400
                   , Gtk.windowTitle := "Sounds of english"
                   , Gtk.containerChild := vbox ]
    -- Gtk.set button [ Gtk.buttonLabel := "Hello World" ]
    -- Gtk.on button Gtk.buttonActivated $ putStrLn "Hello World"
    Gtk.on window Gtk.deleteEvent $ liftIO $ Gtk.mainQuit $> True
    Gtk.widgetShowAll window
    Gtk.mainGUI
