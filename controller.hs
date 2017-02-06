{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (finally)
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)
import Data.ByteString (singleton)
import qualified Data.ByteString as B
import Data.Bits
import Data.Char
import Data.Data (Data, Typeable)
import Data.Word (Word8)
import Data.Default (def)
-- import Graphics.UI.SDL
import Sound.NH.MIDI.Core
import Sound.NH.MIDI.Parse
-- import RawMidi
import Prelude hiding (read)
import System.Hardware.Serialport
import System.IO (BufferMode(..), hGetLine, hWaitForInput, hSetBuffering, stdin, stdout)
import System.Exit (exitSuccess)
import System.Random

red, darkOrange, lightOrange, yellow, green, blue, violet, white :: Word8
red         = 0
darkOrange  = 1
lightOrange = 2
yellow      = 3
green       = 4
blue        = 5
violet      = 6
white       = 7


port = "/dev/ttyUSB0"
baud = CS9600

data Switch
  = Open
  | Close
    deriving (Eq, Ord, Read, Show, Data, Typeable)

-- type Channel = Word8

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

data MidiState = MidiState
    { parser       :: MidiParser
    , getNextWord8 :: IO Char
    , serialOut    :: SerialPort
    , msgTChan     :: TChan String
    }

data SystemState = SystemState
    { songPositionPointer :: Value14
    , currentChannel      :: Word8
    }
    deriving (Show)

mainRandom :: IO ()
mainRandom =
  do s <- openSerial port defaultSerialSettings { commSpeed = baud }
     forever $ do
       c <- randomRIO (0,7::Word8)
       b <- randomIO
       let switch = if b then Close else Open
       send s $ singleton (serializeCommand (Command c switch))
       threadDelay (125 * 100)

mainMidi :: IO ()
mainMidi =
--  withSerial port defaultSerialSettings { commSpeed = baud } $ \s ->
      do putStrLn "starting."
         msg <- newTChanIO

         s <- openSerial port defaultSerialSettings { commSpeed = baud }
{-
         (Right handle_in)     <- openInput "hw:2,0,0" None
         putStrLn "opened MPD32."
         let midiState = MidiState { parser       = parseStatus 0
                                   , getNextWord8 = read handle_in
                                   , serialOut    = s
                                   , msgTChan     = msg
                                   }
         forkIO $ evalStateT (midiLoop processMidi) midiState
         putStrLn "midiLoop processMidi started."
-}
         (Right handle_timeIn) <- openInput "hw:2,0,2" None
         putStrLn "opened MPD32 MIDI Input."
         let midiTimeState = MidiState { parser       = parseStatus 0
                                       , getNextWord8 = read handle_timeIn
                                       , serialOut    = s
                                       , msgTChan     = msg
                                       }

         forkIO $ evalStateT (evalStateT (midiLoop processMidiTime) midiTimeState) (SystemState 0 0)
         putStrLn "listening."
         (forever $ do msg <- atomically $ readTChan msg
                       putStrLn msg) `finally` closeSerial s
--         hGetLine stdin
--         closeSerial s
--         evalStateT (midiLoop (parseStatus 0) (lift $ read handle_in) (elWire s))
--         parseLoop :: (Functor m, MonadIO m) => MidiParser -> m Char -> (MIDI -> m ()) -> m ()

midiLoop :: (Functor m, MonadIO m) => (MIDI -> StateT MidiState m ()) -> StateT MidiState m ()
midiLoop handler =
  forever $
      do ss@MidiState{..} <- get
         w <- lift $ fmap (fromIntegral . ord) (liftIO getNextWord8)
         let (parser', mMidi) = (unMP parser) w
         put $ ss { parser = parser' }
         case mMidi of
           (Just ActiveSense) -> return ()
           (Just midi) -> handler midi
           _ -> return ()

--         parseLoop parser' getNextWord8 handleEvent

processMidiLakes :: MIDI -> StateT MidiState IO ()
processMidiLakes midi =
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


    _ -> do c <- msgTChan <$> get
            liftIO $ atomically $ writeTChan c (show midi)

processMidi :: MIDI -> StateT MidiState IO ()
processMidi midi = do
  c <- msgTChan <$> get
  liftIO $ atomically $ writeTChan c (show midi)
  case midi of
    (NoteOn _ nn _) | nn >= 36 && nn < (36 + 9) ->
      do let cmd = Command (nn - 36) Close
--         liftIO $ print cmd
--         liftIO $ print (serializeCommand cmd)
         s <- serialOut <$> get
         liftIO $ send s $ singleton (serializeCommand cmd)
         return ()
    (NoteOff _ nn _) | nn >= 36 && nn < (36 + 9) ->
      do let cmd = Command (nn - 36) Open
--         liftIO $ print cmd
--         liftIO $ print (serializeCommand cmd)
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
    (NoteOn _ nn _) | nn == 51 -> liftIO exitSuccess
--     _ -> liftIO $ print midi
    _ -> return ()


processMidiTime :: MIDI -> StateT MidiState (StateT SystemState IO) ()
processMidiTime midi =
  do c <- msgTChan <$> get
     liftIO $ atomically $ writeTChan c (show midi)
     ss <- lift get
     case midi of
       (SongPositionPointer v) -> lift $ put (ss { songPositionPointer = v
--                                                 , currentChannel      = 0
                                                 })
       MidiClock               -> lift $ put $ ss { songPositionPointer = succ (songPositionPointer ss) }
       _ -> return ()
     ss <- lift get
     -- liftIO $ atomically $ writeTChan c ("songPositionPointer: " ++ show (songPositionPointer ss))
     if (songPositionPointer ss `mod` 12 == 0)
       then do s <- serialOut <$> get
               let cc = fromIntegral $ (songPositionPointer ss `div` 12) `mod` 8
                   pc = fromIntegral (((pred (fromIntegral cc)) :: Int) `mod` 8)
               liftIO $ atomically $ writeTChan c (show (pc, cc))
               liftIO $ send s $ singleton (serializeCommand (Command pc Open))
               liftIO $ send s $ singleton (serializeCommand (Command cc Close))
               return ()
       else return ()

main :: IO ()
main = mainRandom

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
