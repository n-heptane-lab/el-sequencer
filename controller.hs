{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.ByteString (singleton)
import qualified Data.ByteString as B
import Data.Bits
import Data.Char
import Data.Data (Data, Typeable)
import Data.Word (Word8)
import Data.Default (def)
import Graphics.UI.SDL
import Core
import Parse
import RawMidi
import Prelude hiding (read)
import System.Hardware.Serialport
import System.Random

port = "/dev/ttyUSB0"
baud = CS9600

data Switch
  = Open
  | Close
    deriving (Eq, Ord, Read, Show, Data, Typeable)

data Command = Command
    { channel :: Word8
    , switch  :: Switch
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

serializeCommand :: Command -> Word8
serializeCommand (Command channel switch) =
  (channel `shiftL` 1) .|. (case switch of
                               Open  -> 0
                               Close -> 1)

data SystemState = SystemState
  { parser       :: MidiParser
  , getNextWord8 :: IO Char
  , serialOut    :: SerialPort
  }


mainMidi :: IO ()
mainMidi =
    withSerial port defaultSerialSettings { commSpeed = baud } $ \s ->
      do (Right handle_in) <- openInput "hw:2,0,0" None
         let systemState = SystemState { parser       = parseStatus 0
                                       , getNextWord8 = read handle_in
                                       , serialOut    = s
                                       }
         evalStateT midiLoop systemState
--         evalStateT (midiLoop (parseStatus 0) (lift $ read handle_in) (elWire s))
--         parseLoop :: (Functor m, MonadIO m) => MidiParser -> m Char -> (MIDI -> m ()) -> m ()

midiLoop :: StateT SystemState IO ()
midiLoop =
  forever $
      do ss@SystemState{..} <- get
         w <- lift $ fmap (fromIntegral . ord) getNextWord8
         let (parser', mMidi) = (unMP parser) w
         put $ ss { parser = parser' }
         case mMidi of
           (Just ActiveSense) -> return ()
           (Just midi) -> processMidi midi
           _ -> return ()

--         parseLoop parser' getNextWord8 handleEvent

processMidi :: MIDI -> StateT SystemState IO ()
processMidi midi =
  case midi of
    (NoteOn _ nn _) | nn >= 36 && nn < (36 + 9) ->
      do let cmd = Command (nn - 36) Close
         liftIO $ print cmd
         liftIO $ print (serializeCommand cmd)
         s <- serialOut <$> get
         liftIO $ send s $ singleton (serializeCommand cmd)
         return ()
    (NoteOff _ nn _) | nn >= 36 && nn < (36 + 9) ->
      do let cmd = Command (nn - 36) Open
         liftIO $ print cmd
         liftIO $ print (serializeCommand cmd)
         s <- serialOut <$> get
         liftIO $ send s $ singleton (serializeCommand cmd)
         return ()
    (NoteOn _ nn _) | nn == 45 -> -- all on
      do s <- serialOut <$> get
         liftIO $ send s $ B.pack (map serializeCommand [Command n Close | n <- [0..7]])
         return ()
    (NoteOn _ nn _) | nn == 46 ->
      do s <- serialOut <$> get
         -- all off
         liftIO $ send s $ B.pack (map serializeCommand [Command n Open | n <- [0..7]])

         -- random
         replicateM_ 50 $
           do light <- liftIO $ randomRIO (0,7)
              switch <- do b <- liftIO $ randomIO
                           return $ if b then Close else Open
              liftIO $ send s $ singleton (serializeCommand (Command light switch))
              liftIO $ threadDelay (1000 * 50)
              return ()
         -- all on
         liftIO $ send s $ B.pack (map serializeCommand [Command n Close | n <- [0..7]])
         return ()
    (NoteOn _ nn _) | nn == 47 -> -- rat-ta-ta-tat
      do s <- serialOut <$> get
         liftIO $ replicateM_ 4 $
           do putStrLn "click"
              send s $ singleton (serializeCommand (Command 8 Close))
              threadDelay (1000 * 15)
              send s $ singleton (serializeCommand (Command 8 Open))
              threadDelay (1000 * 150)
         liftIO $ send s $ singleton (serializeCommand (Command 8 Open))
         return ()

    (NoteOn _ nn _) | nn == 48 -> -- dit dit daa
      do s <- serialOut <$> get
         liftIO $
           do send s $ singleton (serializeCommand (Command 8 Close))
              threadDelay (1000 * 15)
              send s $ singleton (serializeCommand (Command 8 Open))
              threadDelay (1000 * 150)

              send s $ singleton (serializeCommand (Command 8 Close))
              threadDelay (1000 * 15)
              send s $ singleton (serializeCommand (Command 8 Open))
              threadDelay (1000 * 150)

              send s $ singleton (serializeCommand (Command 8 Close))
              threadDelay (1000 * 80)
              send s $ singleton (serializeCommand (Command 8 Open))
              threadDelay (1000 * 150)

         liftIO $ send s $ singleton (serializeCommand (Command 8 Open))
         return ()


    _ -> liftIO $ print midi

main :: IO ()
main = mainMidi

{-
mainLights :: IO ()
mainLights =
  withSerial port defaultSerialSettings { commSpeed = baud } $ \s ->
    forever $ do putStrLn "on"
                 send s $ singleton 0x0
                 threadDelay 200000
                 putStrLn "off"
                 send s $ singleton 0xff
                 threadDelay 200000

mainFlame :: IO ()
mainFlame =
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

main :: IO ()
main = mainFlame
-}