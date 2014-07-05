module Main where

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.ByteString
import Data.Char
import Data.Default (def)
import Graphics.UI.SDL
import System.Hardware.Serialport

port = "/dev/ttyUSB0"
baud = CS9600

main :: IO ()
main =
  withInit [InitEverything] $ do
    e <- getError
    print e
    surface <- setVideoMode 640 480 24 []
    withSerial port defaultSerialSettings { commSpeed = baud } $ \s ->
       forever $ do
         event <- waitEvent
         case event of
           (KeyDown keySym) ->
             case symKey keySym of
               SDLK_RETURN -> flameOn s
               _           -> return ()
           (KeyUp keySym) ->
             case symKey keySym of
               SDLK_RETURN -> flameOff s
               _           -> return ()
           _           -> return ()


flameOn :: SerialPort -> IO ()
flameOn s =
  do send s $ singleton 0x1
     return ()

flameOff :: SerialPort -> IO ()
flameOff s =
  do send s $ singleton 0x0
     return ()
