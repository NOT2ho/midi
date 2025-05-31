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
main = saveMidi "C:/Users/i5-32/Desktop/midi/test10.midi"

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

toWord28be :: Int -> [Word8]
toWord28be w =
    if w <= 0x0FFFFFFF then [ fromIntegral (w `shiftR` 21)
    , fromIntegral (w `shiftR` 14 .&. 0b01111111)
    , fromIntegral (w `shiftR` 7 .&. 0b01111111)
    , fromIntegral (w .&. 0b01111111)
    ]
    else [0b11111111, 0b11111111, 0b11111111, 0b01111111]
    -- else error "you wrong"

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

fromInts :: Int -> [Word8]
fromInts i = fromWord16be (fromIntegral i :: Word16)


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
    B.hPut h "MTrk"
    beforeData <- hTell h
    B.hPut h "dd"
    t h
    afterData <- hTell h
    let dataSize = fromIntegral (afterData - beforeData - 4)
    hSeek h AbsoluteSeek beforeData
    putWords h fromWord16be dataSize

-- permutativeMidi :: Int -> B.ByteString
-- permutativeMidi =

vlq :: Int -> [Word8]
vlq i = let b = toWord28be $ fromIntegral i in
            let b' = dropWhile (== 0x00) b in 
            case length b' of
            0 -> [0]
            1 -> b'
            _ -> head b' + 0b10000000 : drop 1 b'

event :: Int -> [Word8] -> [Word8]
event i a = vlq (i * 0x80) ++ a

data Note =
    ON Int
    | OFF Int

note :: Note -> [Word8]
note i =
    case i of
        ON a -> [fromInt 0x90, fromInt (72 + a), fromInt 100]
        OFF a -> [fromInt 0x80, fromInt (72 + a), fromInt 100]

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
            ++ event 4 (note $ ON 1)
            ++ event 16 (note $ OFF 1)
            ++ event 100 (meta EOT)