{-# LANGUAGE RecordWildCards #-}


module Main where

import Data.Int (Int64)
import Graphics.Vty as V
import Graphics.Vty.Image as I -- for image outputs for Picture
import Graphics.Vty.Attributes as A -- for styling images.
import Graphics.Vty.Platform.Windows
import Control.Concurrent 
import Control.Monad (void, when, forever)

sleepSeconds :: Int -> IO () 
sleepSeconds ms = threadDelay (ms * 1000000)

----------------------------
data StopLight = StopLight 
               {arrow :: Bool 
               ,go    :: Bool 
               ,wait  :: Bool 
               ,stop  :: Bool 
               } deriving (Show , Eq)

blackout :: StopLight 
blackout = StopLight {arrow=False,go=False,wait=False,stop=False}
stopLightToImage :: StopLight -> Image 
stopLightToImage stoplight = 
    horizCat 
    [ if arrow stoplight then char defAttr '←' else char defAttr '⚫'
    , if go stoplight then char defAttr '🟢' else char defAttr '⚫'
    , if wait stoplight then char defAttr '🟡' else char defAttr '⚫'
    , if stop stoplight then char defAttr '🔴' else char defAttr '⚫'
    ]
-----------------
titlePage :: Image 
titlePage = vertCat 
        [string defAttr "You are at a stop light simulator."
        ,string defAttr "Press the character 's' to start."
        ,string defAttr "Press the character 'q' to stop at anytime."
        ]

--------------------------------------
main :: IO ()
main = do 
    vty <- mkVty defaultConfig 
    let display = update vty 
    display $ picForImage titlePage
    handleBrainDamage vty 
    
-------------------------------------------------
startStopLight :: Vty -> IO ()
startStopLight vty = do 
    let display = update vty 
    display $ picForImage $ stopLightToImage blackout
    sleepSeconds 1
    display $ picForImage $ stopLightToImage (StopLight {arrow = False , go = True , wait = False, stop = False})
    sleepSeconds 15
    display $ picForImage $ stopLightToImage (StopLight {arrow = False , go = False, wait = True, stop = False})
    sleepSeconds 5
    display $ picForImage $ stopLightToImage (StopLight {arrow = False, go = False, wait = False, stop = True})
    sleepSeconds 5
    display $ picForImage $ stopLightToImage (StopLight {arrow = True , go = False, wait = False, stop = False})
    sleepSeconds 10
    display $ picForImage $ stopLightToImage (StopLight {arrow = False, go = False, wait = False, stop = True})
    sleepSeconds 15
    display $ picForImage $ stopLightToImage (StopLight {arrow = True, go = True, wait = False, stop = False})
    sleepSeconds 10 


    
    
        
    
    

handleBrainDamage :: Vty -> IO () 
handleBrainDamage vty = do 
    event <- nextEvent vty 
    case event of 
        (EvKey (KChar 's') []) -> f vty 

 
        (EvKey (KChar 'q') []) -> shutdown vty 
        _                      -> handleBrainDamage vty 

f :: Vty  -> IO () 
f vty = do 
  startStopLight vty 
  newEvent <- nextEvent vty 
  case newEvent of 
    (EvKey (KChar 'q') []) -> shutdown vty 
    _                      -> f vty 




