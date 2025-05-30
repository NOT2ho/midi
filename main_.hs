{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import qualified Data.ByteString          as B
import Data.Word
import Data.Bits
import Data.Maybe
import Data.List
import Data.Monoid (mempty, mconcat, mappend)
import Control.Applicative
import Control.Monad
import System.IO
import Control.Monad.IO.Class

main :: IO ()
main = saveMidi "/test2.midi"

saveMidi :: String -> IO ()
saveMidi s = midiFileIO s (\h -> B.hPut h (B.pack testMidi ))

midiFileIO ::
  (MonadIO m) =>
  FilePath ->
  (Handle -> IO ()) ->
  m ()
midiFileIO path writeData = liftIO . withBinaryFile path WriteMode $ \h ->
  midiIO h writeData >>= (\h' -> hClose h)


fromWord32be :: Word32 -> [Word8]
fromWord32be w = [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

fromWord24be :: Word32 -> [Word8]
fromWord24be w = [
    fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

fromWord16be :: Word16 -> [Word8]
fromWord16be w = [fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

fromInt :: Int -> Word8
fromInt i = fromIntegral i :: Word8

putWords :: Handle -> (a -> [Word8]) -> a -> IO ()
putWords h f a = B.hPut h $ B.pack (f a)


midiIO ::
    Handle ->
    (Handle -> IO ()) ->
    IO ()
midiIO h t = do
----header-----------------------------
    B.hPut h "MThd"
    putWords h fromWord32be 6
    putWords h fromWord16be 0
    putWords h fromWord16be 1
    putWords h fromWord16be 96
----track------------------------------
    beforeData <- hTell h
    B.hPut h "MTrk"
    insert <- hTell h
    B.hPut h "dd"
    t h
    afterData <- hTell h
    let dataSize = fromIntegral (afterData - beforeData - 8)
    hSeek h AbsoluteSeek insert
    putWords h fromWord16be dataSize

-- permutativeMidi :: Int -> B.ByteString
-- permutativeMidi =

event :: Int -> [Word8] -> [Word8]
event i a = fromInt (i * 0x80) : a

data Note =
    ON Int
    | OFF Int

note :: Note -> [Word8]
note i =
    case i of
        ON a -> [fromInt 0x90, fromInt (48 + a), fromInt 64]
        OFF a -> [fromInt 0x80, fromInt (48 + a), fromInt 64]

data Meta = 
    Tempo 
    | TimeSig 
    | KeySig 
    | EOT

meta :: Meta -> [Word8]
meta m = 
    case m of
        Tempo -> [0xFF, 0x51, 0x03, 0x07, 0xa1, 0x20]
        TimeSig -> [0xFF, 0x58, 0x04, 0x04, 0x02, 0x18, 0x08] 
        KeySig -> [0xFF, 0x59, 0x02, 0x00, 0x00] 
        EOT -> [0xFF, 0x2F, 0x00]

testMidi :: [Word8]
testMidi = concatMap (event 0) [meta Tempo, meta TimeSig, meta KeySig]
            ++ event 0 (note $ ON 1)
            ++ event 4 (note $ OFF 1)
            ++ event 5 (meta EOT)