{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import qualified Data.ByteString as B
import Data.Word
import Data.Bits
import Data.List
import System.IO
import Control.Monad.IO.Class

main :: IO ()
main = do
    let path = "/"
    saveMidi (path ++ "0.midi") permutative12
    saveMidi (path ++ "1.midi") permutative12'
    saveMidi (path ++ "2.midi") permutative12''
    saveMidi (path ++ "3.midi") permutative12'''
    saveMidi (path ++ "c0.midi") chord12
    saveMidi (path ++ "c1.midi") chord12'
    saveMidi (path ++ "c2.midi") chord12''
    saveMidi (path ++ "c3.midi") chord12'''

saveMidi :: String -> [Word8] -> IO ()
saveMidi s w = midiFileIO s w (\h -> B.hPut h (B.pack w))

midiFileIO ::
    (MonadIO m) =>
    FilePath ->
    [Word8] ->
    (Handle -> IO ()) ->
    m ()
midiFileIO path w writeData = liftIO . withBinaryFile path WriteMode $ \h ->
  midiIO h w writeData >>= (\h' -> hClose h)


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
    [Word8] ->
    (Handle -> IO ()) ->
    IO ()
midiIO h w t = do
----header-----------------------------
    B.hPut h "MThd"
    putWords h fromWord32be 6
    putWords h fromWord16be 0
    putWords h fromWord16be 1
    putWords h fromWord16be 96
----track------------------------------
    B.hPut h "MTrk"
    B.hPut h (B.pack $ fromInts (length w))
    t h

permutativeMidi :: Int -> [Word8]
permutativeMidi i =
    concatMap (event 0) [meta Tempo, meta TimeSig, meta KeySig]
    ++ concatMap (createNote 0) (concat $ subsequences [0..i])
    ++ event 0 (meta EOT)

createNote :: Int -> Int -> [Word8]
createNote l i = event l (note $ ON i) ++ event (l+1) (note $ OFF i)

chordNote :: [Int] -> [Word8]
chordNote (i:is) = event 4 (note $ ON i) ++ concatMap (event 0 . note . ON) is
            ++ event 4 (note $ OFF i) ++ concatMap (event 0 . note . OFF) is

sorted :: Int -> [[Int]]
sorted i = sortOn length $ subsequences [0..i]

byLength :: [[Int]] -> [[Int]]
byLength l = drop 2 $  foldl1 (++) `map` groupBy (\a b -> length a == length b ) l

byLength' :: [[Int]] -> [[[Int]]]
byLength' l=  drop 2  (groupBy (\a b -> length a == length b ) l)

chord12 :: [Word8]
chord12 = let [a,b,c,d,e,f,g,h,j,k,l] = byLength' $ sorted 11
        in concatMap (event 0) [meta Tempo, meta TimeSig, meta KeySig]
        ++ concatMap chordNote (concat [a,b,c])
        ++ event 0 (meta EOT)

chord12' :: [Word8]
chord12' = let [a,b,c,d,e,f,g,h,j,k,l] = byLength' $ sorted 11
        in concatMap (event 0) [meta Tempo, meta TimeSig, meta KeySig]
        ++ concatMap chordNote (concat [d,e])
        ++ event 0 (meta EOT)

chord12'' :: [Word8]
chord12'' = let [a,b,c,d,e,f,g,h,j,k,l] = byLength' $ sorted 11
        in concatMap (event 0) [meta Tempo, meta TimeSig, meta KeySig]
        ++ concatMap chordNote (concat [f,g])
        ++ event 0 (meta EOT)

chord12''' :: [Word8]
chord12''' = let [a,b,c,d,e,f,g,h,j,k,l] = byLength' $ sorted 11
        in concatMap (event 0) [meta Tempo, meta TimeSig, meta KeySig]
        ++ concatMap chordNote (concat [h,j,k,l])
        ++ event 0 (meta EOT)

permutative12 :: [Word8]
permutative12 = let [a,b,c,d,e,f,g,h,j,k,l] = byLength $ sorted 11
                in concatMap (event 0) [meta Tempo, meta TimeSig, meta KeySig]
                    ++ concatMap (createNote 5) a
                    ++ concatMap (createNote 3) b
                    ++ concatMap (createNote 3) c
                    ++ event 0 (meta EOT)

permutative12' ::[Word8]
permutative12' = let [a,b,c,d,e,f,g,h,j,k,l] = byLength $ sorted 11
                in concatMap (event 0) [meta Tempo, meta TimeSig, meta KeySig]
                   ++ concatMap (createNote 2) d
                    ++ concatMap (createNote 1) e
                    ++ event 0 (meta EOT)

permutative12'' ::[Word8]
permutative12'' = let [a,b,c,d,e,f,g,h,j,k,l] = byLength $ sorted 11
                in concatMap (event 0) [meta Tempo, meta TimeSig, meta KeySig]
                    ++ concatMap (createNote 0) (f ++ g)
                    ++ event 0 (meta EOT)

permutative12''' ::[Word8]
permutative12''' = let [a,b,c,d,e,f,g,h,j,k,l] = byLength $ sorted 11
                in concatMap (event 0) [meta Tempo, meta TimeSig, meta KeySig]
                    ++ concatMap (createNote 0) (concat [h,j,k,l])
                    ++ event 0 (meta EOT)

vlq :: Int -> [Word8]
vlq i = let b = toWord28be $ fromIntegral i in
            let b' = dropWhile (== 0x00) b in
            case length b' of
            0 -> [0]
            1 -> b'
            _ -> map (.|. 0b10000000) (init b') ++ [last b']


event :: Int -> [Word8] -> [Word8]
event i a = vlq (i * 2) ++ a

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
            ++ event 0 (note $ ON 1)
            ++ event 1 (note $ OFF 1)
            ++ event 0 (note $ ON 2)
            ++ event 1 (note $ OFF 2)
            ++ event 0 (note $ ON 4)
            ++ event 1 (note $ OFF 4)
            ++ event 1 (meta EOT)