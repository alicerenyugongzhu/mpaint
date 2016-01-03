-- 
-- module Unpack - Unpacks an MP3 bitstream into parts.
--
-- This code is part of the Experimental Haskell MP3 Decoder, version 0.0.1.
-- Copyright (c) 2008 Bjorn Edstrom <be@bjrn.se>
--
-- This software is provided 'as-is', without any express or implied
-- warranty. In no event will the authors be held liable for any damages
-- arising from the use of this software.
--
-- Permission is granted to anyone to use this software for any purpose,
-- including commercial applications, and to alter it and redistribute it
-- freely, subject to the following restrictions:
--
--    1. The origin of this software must not be misrepresented; you must not
--    claim that you wrote the original software. If you use this software
--    in a product, an acknowledgment in the product documentation would be
--    appreciated but is not required.
--
--    2. Altered source versions must be plainly marked as such, and must not be
--    misrepresented as being the original software.
--
--    3. This notice may not be removed or altered from any source
--    distribution.
--
-- Small Warning: Unpacking the bitstream is the messiest part of
-- MP3-decoding, so this code is not the prettiest ever written.
--
-- We unpack the bitstream in two steps.
--
-- 1) The bytes in an MP3 are grouped in blocks known as frames.
--    A frame consists of a header, side data, and main data.
--    The header has some information about the properties of the
--    audio. The side data contains some information about how
--    the audio should be decoded, but most bits describe
--    how to unpack the main data.
--
--    The main data within a frame do not necessarily
--    correspond to the header and side data. The first step is to
--    take the "physical" frames and group them into "logical
--    frames", consisting of one header, side data, and
--    bytes of main data.
--
-- 2) We then take the logical frame and unpack the main data,
--    according to the parameters saved in the side data.
--    The main data consists of two "granules", containing one
--    or two channels each. Each of these channel granules are
--    later decoded (more or less) separately.
--
--    Each channel granule consists of scale information, and
--    huffman coded audio.
--

module Codec.Audio.MP3.Unpack (
    mp3Seek
   ,mp3Unpack
   ,MP3Bitstream(..)
) where


-- binary-strict from Hackage.
import qualified Data.Binary.Strict.BitGet as BG
import Data.Binary.Strict.Get

import qualified Data.ByteString as B
import Data.Word
import Data.Bits
import Control.Monad (replicateM)

import Codec.Audio.MP3.Huffman
import Codec.Audio.MP3.Types
import Codec.Audio.MP3.Tables

import Debug.Trace

-- 
-- Exported types.
--

data MP3Bitstream = MP3Bitstream {
    bitstreamStream :: B.ByteString,
    bitstreamBuffer :: [Word8]
} deriving (Show)

-- 
-- The types below are only used in this module during unpacking.
-- Important information needed elsewhere is copied to another
-- data structure described in Types.hs
--

data MP3LogicalFrame = MP3LogicalFrame {
    logicalHeader :: MP3FrameHeader,
    logicalSide   :: MP3SideInfo,
    logicalData   :: [Word8]
} deriving (Show)

data MP3HuffmanData = MP3HuffmanData {
    huffmanBigValues     :: Int,
    huffmanRegionLengths :: (Int, Int, Int),
    huffmanTable         :: (Int, Int, Int),
    huffmanCount1Table   :: Int
} deriving (Show)

data MP3ScaleData = MP3ScaleData {
    scaleGlobalGain         :: Double,
    scaleLengths            :: (Int, Int),
    scaleSubblockGain       :: (Double, Double, Double),
    scaleScalefacScale      :: Int,
    scalePreflag            :: Int 
} deriving (Show)

data MP3SideData = MP3SideData {
    sideHuffman      :: MP3HuffmanData,
    sideScalefactor  :: MP3ScaleData,
    sidePart23Length :: Int,
    sideBlocktype    :: Int,
    sideBlockflag    :: BlockFlag
} deriving (Show)
 
data MP3SideInfo = MP3SideInfo1Ch { sideMaindata    :: Int, 
                                    sideScfsi       :: Int, 
                                    sideGranule0ch0 :: MP3SideData, 
                                    sideGranule1ch0 :: MP3SideData }
                 | MP3SideInfo2Ch { sideMaindata    :: Int,
                                    sideScfsi       :: Int,
                                    sideGranule0ch0 :: MP3SideData,
                                    sideGranule0ch1 :: MP3SideData,
                                    sideGranule1ch0 :: MP3SideData,
                                    sideGranule1ch1 :: MP3SideData } 
                 deriving (Show)


-- 
-- Generic utilities.
--

flattenTuple :: [(a, a)] -> [a]
flattenTuple []           = []
flattenTuple ((x0,x1):xs) = x0 : x1 : flattenTuple xs

applyTuple :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
applyTuple f1 f2 (x, y) = (f1 x, f2 y)

toBool 0 = False
toBool _ = True

padWith :: Int -> a -> [a] -> [a]
padWith n padding xs = xs ++ replicate (n - length xs) padding

--
-- Utilities for dealing with bits.
--

-- binery-strict is broken on some some setups so we use the safe but slow
-- method of constructing integers from individual bits. BG.getBit works.
getBitsFixed 0 = return 0
getBitsFixed n = do bit0 <- BG.getBit >>= return . fromEnum
                    rest <- getBitsFixed (n-1)
                    return $ bit0 `shiftL` (n-1) .|. rest


toWord32 :: [Word8] -> Word32
toWord32 (b0:b1:b2:b3:[]) =   (fromIntegral b3)
                            + (fromIntegral b2) * 0x100
                            + (fromIntegral b1) * 0x10000
                            + (fromIntegral b0) * 0x1000000
toWord32 _                = 0

bsGetWord32 :: B.ByteString -> Word32
bsGetWord32 = toWord32 . B.unpack . (B.take 4)

-- Extract an interval of bits from a larger integer.
-- bitInterval '11111111 11100000 00000000 00000000' 21 11 = '11111111 111'
bitInterval :: (Bits a) => a -> Int -> Int -> a
bitInterval word start size = (word `shiftR` start) .&. 
                              ((1 `shiftL` size)-1)


--
-- mp3ParseFrameHeader
--
-- Get the important (needed for decoding) stuff out of a frame header.
--
-- Return values:
--
-- The function will return Nothing if the frame is malformed,
-- or if it's a valid header but a format not supported by this
-- decoder, such as MP2.
--
-- Info:
--
-- The first 11 bits of a correct header is the sync word, 0x7ff.
-- IS and MS are two stereo processing modes used to reduce
-- the size of a Joint Stereo MP3. Info in Decoder.hs.
--
mp3ParseFrameHeader :: Word32 -> Maybe MP3FrameHeader
mp3ParseFrameHeader bits
  | bitsSync       /= 0x7ff = Nothing
  | bitsMpeg       /= 3     = Nothing -- We only support MPEG1 (bits=3)
  | bitsLayer      /= 1     = Nothing -- We only support Layer3 (bits=1)
  | bitsBitrate    == 0 || 
    bitsBitrate    == 15    = Nothing
  | bitsSR         == 3     = Nothing
  | otherwise               = Just $ MP3FrameHeader bitrate samplerate 
                                                    channels crc padding ms is
  where
    bitrate      = [  0,  32,  40,  48,  56,  64,  
                     80,  96, 112, 128, 160, 192, 
                    224, 256, 320, 0]      !! (fromIntegral bitsBitrate)
    samplerate   = [44100, 48000, 32000]   !! (fromIntegral bitsSR)
    channels     = [Stereo, JointStereo, 
                    DualChannel, Mono]     !! (fromIntegral bitsCh)
    crc          = [True, False]           !! (fromIntegral bitsCRC)
    padding      = fromIntegral bitsPadding
    (is, ms)     = [(False, False), (True, False), 
                    (False, True), (True, True)] !! (fromIntegral bitsMode)
    bitsSync     = bitInterval bits 21 11
    bitsMpeg     = bitInterval bits 19  2
    bitsLayer    = bitInterval bits 17  2
    bitsCRC      = bitInterval bits 16  1
    bitsBitrate  = bitInterval bits 12  4
    bitsSR       = bitInterval bits 10  2
    bitsPadding  = bitInterval bits  9  1
    bitsCh       = bitInterval bits  6  2
    bitsMode     = bitInterval bits  4  2
    --getbits      = bitInterval bits


-- 
-- mp3FrameLength
--
-- How long a physical frame is, in bytes.
--
-- This simplified expression takes the frame length, side info length
-- and main data into account.
--
mp3FrameLength :: MP3FrameHeader -> Int
mp3FrameLength header = let br      = headBitrate    header
                            sr      = headSampleRate header
                            padding = headPadding    header
                        in (144 * 1000 * br) `div` sr + padding


--
-- mp3ParseSideInfo
--
-- Parse the (32 for stereo, 17 for mono) side data bytes.
--
-- Return values:
--
-- The function will return Nothing if
--   The input buffer is too short.
--   The input buffer is of the correct size, but contains invalid data.
-- The function will return a Just MP3SideInfo otherwise.
--
-- Info:
--
-- Many of the bits in the side info are indices to tables defined
-- in the MP3 standard, and are thus not interesting in themselves.
-- In this MP3 decoder, the design choice is to parse these bits
-- as early as possible.
--
mp3ParseSideInfo :: MP3FrameHeader -> [Word8] -> Maybe MP3SideInfo
mp3ParseSideInfo header buffer = 
  case BG.runBitGet stream (if mono then getMono 
                                    else getStereo) of
       Right ret -> ret
       otherwise -> Nothing
  where
    stream     = B.pack buffer
    mono       = Mono == headChannels header
    samplerate = headSampleRate header

    getMono = do dataptr     <- getBitsFixed 9
                 private     <- getBitsFixed 5 
                 scfsi       <- getBitsFixed 4 
                 granule0ch0 <- bitSideData
                 granule1ch0 <- bitSideData
                 return $ constr (fromIntegral dataptr) 
                                 scfsi 
                                 granule0ch0 
                                 granule1ch0
        where
            constr dataptr scfsi (Just g00) (Just g01) = 
               Just $ MP3SideInfo1Ch dataptr scfsi g00 g01
            constr _ _ _ _ = Nothing

    getStereo = do dataptr     <- getBitsFixed 9
                   private     <- getBitsFixed 3
                   scfsi       <- getBitsFixed 8 -- 4 bits per channel
                   granule0ch0 <- bitSideData
                   granule0ch1 <- bitSideData
                   granule1ch0 <- bitSideData
                   granule1ch1 <- bitSideData
                   return $ constr (fromIntegral dataptr)
                                   scfsi 
                                   granule0ch0 
                                   granule0ch1 
                                   granule1ch0 
                                   granule1ch1
        where
            constr dataptr scfsi (Just g00) (Just g01) 
                                 (Just g10) (Just g11) = 
                Just $ MP3SideInfo2Ch dataptr scfsi g00 g01 g10 g11
            constr _ _ _ _ _ _ =  Nothing

    validateTable n
        | n == 4 || n == 14 = False
        | otherwise         = True

    toBlockflag mixedflag blocktype
        | mixedflag == True                    = MixedBlocks
        | mixedflag == False && blocktype == 2 = ShortBlocks
        | otherwise                            = LongBlocks

    bitSideData = do 
        -- To parse side data (this): flag.
        -- To parse main data: part23.
        -- To parse main data (huffman): bigvalues, table0-2.
        -- To parse main data (scales): scalelengths.
        -- For decoding the audio in Decoder.hs: globalgain, 
        --     blocktype, blockflag subgain0-2.
        part23        <- getBitsFixed 12
        bigvalues     <- getBitsFixed 9
        globalgainb   <- getBitsFixed 8
        let globalgain = mp3FloatRep1 globalgainb
        scalelengths  <- getBitsFixed 4 >>= return . (tableSlen !!)
        flag          <- BG.getBit
        blocktype     <- getBitsFixed (if flag then 2 else 0)
        mixed         <- getBitsFixed (if flag then 1 else 0) >>= 
                             return . toEnum
        let blockflag = toBlockflag mixed blocktype
        table0        <- getBitsFixed 5
        table1        <- getBitsFixed 5
        table2        <- getBitsFixed (if flag then 0 else 5)
        subgain0b     <- getBitsFixed (if flag then 3 else 0)
        subgain1b     <- getBitsFixed (if flag then 3 else 0)
        subgain2b     <- getBitsFixed (if flag then 3 else 0)
        let subgain0  = mp3FloatRep2 subgain0b
            subgain1  = mp3FloatRep2 subgain1b
            subgain2  = mp3FloatRep2 subgain2b

        -- To parse main data (huffman): r0len, r1len, r2len.
        -- This computation is slightly involved.
        regionabits   <- getBitsFixed (if flag then 0 else 4)
        regionbbits   <- getBitsFixed (if flag then 0 else 3)
        let racnt     = if flag then (if blocktype == 2 then 8 else 7)
                                else regionabits
            rbcnt     = if flag then 20 - racnt
                                else regionbbits
            sbTable   = tableScaleBandBoundary samplerate
            r1bound   = sbTable $ racnt + 1
            r2bound   = sbTable $ racnt + 1 + rbcnt + 1
            bv2       = bigvalues*2
            r0len     = if blocktype == 2 
                           then min bv2 36
                           else min bv2 r1bound
            r1len     = if blocktype == 2 
                           then min (bv2-r0len) 540 
                           else min (bv2-r0len) (r2bound - r0len) 
            r2len     = if blocktype == 2 
                           then 0   
                           else bv2 - (r0len + r1len)

        -- To parse main data (huffman): count1table.
        -- To parse the main data (scales): scalefacscale, preflag.
        preflag       <- BG.getBit >>= return . fromEnum
        scalefacbit   <- BG.getBit
        let scalefacscale = if scalefacbit then 1 else 0      
        count1table   <- BG.getBit >>= return . fromEnum

        -- huffdata has data needed by the Huffman decoder.
        -- scaledata has data needed for parsing the scale data,
        -- and it also has a few parameters needed by the decoder
        -- in Decoder.hs.
        let huffdata  = MP3HuffmanData bigvalues (r0len, r1len, r2len)
                                       (table0, table1, table2) count1table
            scaledata = MP3ScaleData globalgain scalelengths
                                     (subgain0, subgain1, subgain2)
                                     scalefacscale preflag
            valid = if (validateTable table0) && 
                       (validateTable table1) &&
                       (if flag then True else validateTable table2) 
                    then True else False
        return $ if valid then Just (MP3SideData huffdata scaledata part23 
                                                 blocktype blockflag)
                          else Nothing


-- mp3PeekDataPtr takes an unparsed ByteString and returns the dataptr
-- (see mp3ParseSideInfo).
mp3PeekDataPtr :: B.ByteString -> Maybe Int
mp3PeekDataPtr bs =
    case mp3ParseFrameHeader (toWord32 (B.unpack (B.take 4 bs))) of
         Nothing     -> Nothing
         Just header -> let len = 4 + if headCRC header then 2 else 0
                        in case BG.runBitGet (B.drop len bs) bitGetter of
                                Right ret -> Just ret
                                otherwise -> Nothing
    where
        bitGetter = do getBitsFixed 9

-- 
-- Warning: This decoder treats some bits as floatings points.
-- The specification instead treats them as integers. Se discussion
-- at www.bjrn.se/mp3dec
--
-- The side info has two floating point representations
--
-- Representation 1 maps an 8 bit value to the range 0.0 - 2435.5.
--
-- Representation 2 maps a 3 bit value to the range 
-- [1, 0.25, 0.0625, 0.01563, 0.00391, 0.00098, 0.00024, 0.00006]
--
mp3FloatRep1 :: Int -> Double
mp3FloatRep1 n = 2.0 ** (0.25 * (fromIntegral n - 210))

mp3FloatRep2 :: Int -> Double
mp3FloatRep2 n = 2.0 ** (0.25 * (fromIntegral (-n * 8)))

-- 
-- mp3DecodeHuffman
--
-- Decodes the Huffman data to audio (in the frequency domain).
--
-- The huffman coded bits are grouped in five regions, in
-- the 576 frequency bands:
--
-- | region 0 | region 1 | region 2 | count1 region | zero |
--
-- region0-2 are collectively known as the "big values" regions, as
-- the decoded bits are large (-8206 - 8206). These regions represent the
-- lower frequency bands.
--
-- The count 1 region represent the higher frequencies, and can only take
-- on small values (-1 to 1). As the ear is insensitive in these
-- regions, not as much information is required to represent these
-- frequencies.
--
-- The zero region are the frequencies so high they have been removed
-- by the encoder. These frequencies have magnitude 0.
--
-- Different Huffman tables are used by the different regions for better
-- compression. (table1, table2, table3) correspond to the big values
-- regions, while tableC1 is for the count1 region. We call the three first
-- tables the "normal" tables. A normal table outputs two frequency samples.
-- We call the count1 table the quad table, as it outputs four frequency
-- samples.
--
-- The normal tables are decoded as follows:
--
-- 1) Consume input bits until we get a hit in the right Huffman tree. 
--    We now have a tuple of two values (x,y), and also the linbits value 
--    described further below.
-- 2) If x == 15, read linbits bits and add to x. This makes sure the
--    frequency sample can be large, if necessary.
-- 3) If x /= 0, get one bit. If this bit is one, x is negative, otherwise
--    positive.
-- 4) Do step 2 and 3 for y.
--
-- The quad table is decoded as follows:
--
-- 1) Consume input bits until we get a hit in the right Huffman tree.
--    We now have a 4-tuple (x,w,x,y).
-- 2) If v /= 0, get one bit. If this bit is one, v is negative, otherwise
--    positive.
-- 3) Do step 2 for w, x, y.
--
-- The length, in number of output samples, for region0-2 is r0len to 
-- r2len respectively.
-- The length, in number of output samples, for the quad region is not 
-- known explicitly, but is calculated from bitlength of the main data 
-- and the number of bits read when decoding the big values regions.
--
-- TODO:
--
-- This code can be optimized significantly, without hurting readability
-- too much. Instead of walking the tree, consuming one bit at a time
-- until we find a hit in the tree, a large lookup table can be used. 
-- Consider a Huffman tree where the longest code word is N bits. We
-- create a lookup table of 2^N elements, where all code words shorter
-- than N bits are padded with all bitstrings until they are of length
-- N. To use the table, we peek N bits in the bitstream, and use these
-- bits as an index to the lookup table. There we find the length of the
-- code word (so we can throw away the correct number of bits), and the 
-- value.
--
-- Say we have the following Huffman table:
--
-- code word    value
-- 0            a
-- 10           b
-- 111          c
--
-- The corresponding lookup-table will look like this:
--
-- table[000] = (a, 1)
-- table[001] = (a, 1)
-- table[010] = (a, 1)
-- table[011] = (a, 1)
-- table[100] = (b, 2)
-- table[101] = (b, 2)
-- table[110] = Null
-- table[111] = (c, 3)
--
-- To use the table, we peek 3 bits, checks the table. If the result is
-- (b, 2), we throw away 2 bits, and start over.
--
-- For tables where the longest code word is large, real world 
-- decoders use a technique where the lookup table contains "pointers"
-- to other parts of the table, to handle the rare cases where the
-- code words are very long.
--

--mp3DecodeHuffman huffdata part23len part2len = 
mp3DecodeHuffman huffdata huffbitlength =
  do (reg0, bitcount0) <- decodeRegion r0len table1 
     (reg1, bitcount1) <- decodeRegion r1len table2 
     (reg2, bitcount2) <- decodeRegion r2len table3 
     let bitsread      = bitcount0 + bitcount1 + bitcount2
         rqlen         = huffbitlength - bitsread - 1
     regQ              <- decodeRegionQ tableC1 rqlen [] 
     return $ reg0 ++ reg1 ++ reg2 ++ regQ
  where
    (r0len, r1len, r2len)      = huffmanRegionLengths huffdata
    (table1, table2, table3)   = huffmanTable huffdata
    tableC1                    = huffmanCount1Table huffdata

    -- We divide reglen by 2 as each normal Huffman tree has 2 output
    -- samples. The stuff after >>= takes the list of
    -- ((sample0,sample1),bitsread) and constructs a new list
    -- [sample0,sample1,...] as well as a sum of the bits read.
    decodeRegion reglen tablen = 
        replicateM (reglen `div` 2) (decodeOne tablen) 
        >>= return . (applyTuple flattenTuple sum) . unzip

    signBits x        = if x > 0 then 1 else 0
    linBits x linbits = if x == 15 && linbits > 0 then linbits else 0

    decodeOne 0      = return $ ((0, 0), 0)
    decodeOne tablen = 
        do let (table, linbits)  = huffmanDecodeTable tablen
           mval                 <- huffmanLookupM BG.getBit table
           case mval of
                Nothing             -> error "mp3DecodeHuffman"
                Just ((x, y), bitn) -> do
                   let bxlin = linBits x linbits
                       bylin = linBits y linbits
                       bxsgn = signBits x
                       bysgn = signBits y
                   xlin <- getBitsFixed bxlin
                   xsgn <- getBitsFixed bxsgn >>= return . signMul
                   ylin <- getBitsFixed bylin
                   ysgn <- getBitsFixed bysgn >>= return . signMul
                   let x' = (x + xlin) * xsgn 
                       y' = (y + ylin) * ysgn 
                       bitn2 = bxlin + bylin + bxsgn + bysgn
                   return $ ((x', y'), bitn+bitn2)

    signMul 1 = (-1)
    signMul 0 = 1

    decodeOneQuad tablen = 
        do let table = huffmanDecodeTableQuad tablen
           mval     <- huffmanLookupM BG.getBit table
           case mval of
                Nothing                   -> return ((0,0,0,0),0)
                Just ((v, w, x, y), bitn) -> do
                   vsgn  <- getBitsFixed (signBits v) >>= return . signMul
                   wsgn  <- getBitsFixed (signBits w) >>= return . signMul
                   xsgn  <- getBitsFixed (signBits x) >>= return . signMul
                   ysgn  <- getBitsFixed (signBits y) >>= return . signMul
                   let v' = v * vsgn 
                       w' = w * wsgn 
                       x' = x * xsgn 
                       y' = y * ysgn 
                       bitn2 = (signBits v) + (signBits w) + 
                               (signBits x) + (signBits y)
                   return $ ((v', w', x', y'), bitn+bitn2)

    decodeRegionQ tablen bitsrem accum
      | bitsrem <= 0  = if bitsrem == -1 
                           then do return $ accum
                           -- Some old encoders give incorrect length of
                           -- the quad region. Most decoders handle this,
                           -- but we won't (as of 0.0.1).
                           else error "Malformed MP3, aborting. See Unpack.hs"
      | otherwise     = 
          do ((v,w,x,y), bitn) <- decodeOneQuad tablen
             r                 <- decodeRegionQ tablen (bitsrem - bitn) 
                                                       (accum ++ [v,w,x,y])
             return $ r


-- Get regular huffman table from index. This function is safe, since
-- mp3ParseSideInfo ensures the table index is valid.
huffmanDecodeTable :: Int -> (HuffmanTree (Int, Int), Int)
huffmanDecodeTable n = (huffmanFromList table, linbits)
    where
        (table, linbits) = tableHuffman !! n

-- Get quad huffman table from index. This function is safe, as the
-- input is only 1 bit and can only take two values.
huffmanDecodeTableQuad :: Int -> HuffmanTree (Int, Int, Int, Int)
huffmanDecodeTableQuad 0 = huffmanFromList (tableHuffmanQuad !! 0)
huffmanDecodeTableQuad 1 = huffmanFromList (tableHuffmanQuad !! 1)

-- 
-- mp3ParseRawScalefactors
-- 
-- More information about the scale factors can be found in Decoder.hs.
-- Here a short description is sufficient: This function takes
-- bits as input and outputs two list of integers. These integers
-- are often parsed from the bits. Sometimes they are copied from a
-- previously parsed list, if the encoder notices regions of lists
-- are identical.
--
-- We have to count the number of bits read to successfully decode
-- the Huffman data.
--
mp3ParseRawScalefactors blocktype blockflag preflag slen1 slen2 scfsi gran0
    | blocktype == 2 && blockflag == MixedBlocks =
        do scaleL0 <- replicateM 8 (getBitsFixed slen1)
           scaleS0 <- replicateM 3 (replicateM 3 (getBitsFixed slen1))
           scaleS1 <- replicateM 7 (replicateM 3 (getBitsFixed slen2))
           let bitsread = 8*slen1 + 3*3*slen1 + 7*3*slen2
               scaleS   = [[0,0,0],[0,0,0],[0,0,0]]++scaleS0++scaleS1
           return $ (padWith 22 0 scaleL0, 
                     padWith 22 [0,0,0] scaleS, bitsread)
    | blocktype == 2 =
        do scaleS0 <- replicateM 6 (replicateM 3 (getBitsFixed slen1))
           scaleS1 <- replicateM 6 (replicateM 3 (getBitsFixed slen2))
           let bitsread = 6*3*slen1 + 6*3*slen2
           return $ ([], padWith 22 [0,0,0] (scaleS0++scaleS1), bitsread)
    | otherwise =
        do band0 <- if copyband0 then return $ take 6 gran0           
                                 else replicateM 6 (getBitsFixed slen1)
           band1 <- if copyband1 then return $ take 5 (drop 6 gran0) 
                                 else replicateM 5 (getBitsFixed slen1)
           band2 <- if copyband2 then return $ take 5 (drop 11 gran0) 
                                 else replicateM 5 (getBitsFixed slen2)
           band3 <- if copyband3 then return $ take 5 (drop 16 gran0) 
                                 else replicateM 5 (getBitsFixed slen2)
           let bitsread = 6*(if copyband0 then 0 else slen1) +
                          5*(if copyband1 then 0 else slen1) +
                          5*(if copyband2 then 0 else slen2) +
                          5*(if copyband3 then 0 else slen2)
               scalefac  = band0++band1++band2++band3++[0] --Padding
           return $ (scalefac,[[]],bitsread)
    where
          
        copyband0 = not (null gran0) && toBool (scfsi .&. 8)
        copyband1 = not (null gran0) && toBool (scfsi .&. 4)
        copyband2 = not (null gran0) && toBool (scfsi .&. 2)
        copyband3 = not (null gran0) && toBool (scfsi .&. 1)


-- Above parses the scale factors to bits. This functions takes the bits
-- and converts them to doubles according to the correct floating point
-- representation. (See discussion at mp3FloatRep1 above).
--
-- Preflag is simply a predefined table that may be set to give the 
-- higher scale factors a larger range than 4 bits.
mp3UnpackScaleFactors large small preflag scalefacbit =
    let large'  = if preflag == 0 then large
                                  else zipWith (+) large tablePretab
        large'' = map floatFunc large'
        small'  = map (map floatFunc) small
    in (large'', small')
    where
        floatFunc = mp3FloatRep3 scalefacbit

-- Two different floating point representations can be used for the scale
-- factors. (See discussion at mp3FloatRep1).
mp3FloatRep3 :: Int -> Int -> Double
mp3FloatRep3 0 n = 2.0 ** ((-0.5) * (fromIntegral n))
mp3FloatRep3 1 n = 2.0 ** ((-1)   * (fromIntegral n))


-- 
-- mp3ParseMainData
--
-- For stereo, the main data is grouped as follows:
--
--   scale,   granule 0 channel 0
--   huffman, granule 0 channel 0
--   scale,   granule 0 channel 1
--   huffman, granule 0 channel 1
--   scale,   granule 1 channel 0
--   huffman, granule 1 channel 0
--   scale,   granule 1 channel 1
--   huffman, granule 1 channel 1
--
-- For mono, the main data is grouped as follows.
--
--   scale,   granule 0 channel 0
--   huffman, granule 0 channel 0
--   scale,   granule 1 channel 0
--   huffman, granule 1 channel 0
--
-- scale is parsed by mp3ParseRawScaleFactors
-- huffman is parsed by mp3DecodeHuffman
--
-- Return values:
--
-- If the logical frame is too short or invalid, the function returns Nothing.
-- Otherwise it returns Just MP3Data.
--

mp3ParseMainData :: MP3LogicalFrame -> Maybe MP3Data
mp3ParseMainData (MP3LogicalFrame header side maindata) = parse
  where
    parse = 
        case BG.runBitGet (B.pack maindata)
                          (if mono then doAllMono else doAllStereo) of
             Right ret -> Just ret
             Left err  -> Nothing

    mono   = Mono == headChannels header
    scfsi  = sideScfsi side
    scfsi0 = scfsi `shiftR` 4
    scfsi1 = scfsi .&. 0xf

    doOne (MP3SideData huffdata scaledata part23 bt bf) scfsi gran0 =
        do let (slen1, slen2) = scaleLengths scaledata
               pre            = scalePreflag scaledata
               scalefacbit    = scaleScalefacScale scaledata

           (scaleL, scaleS, bitsread) <- 
               mp3ParseRawScalefactors bt bf pre slen1 slen2 scfsi gran0

           let (scaleFacLarge, scaleFacSmall) =
                   mp3UnpackScaleFactors scaleL scaleS pre scalefacbit
                -- Parameters to the IS Stereo decoder are saved in the 
                -- higher unused scale factors.
               (isLarge, isSmall) = (scaleL, scaleS)

           maindata <- mp3DecodeHuffman huffdata (part23-bitsread)

           return $ (scaleL,MP3DataChunk bt bf 
                                         (scaleGlobalGain scaledata)
                                         (scaleSubblockGain scaledata)
                                         scaleFacLarge
                                         scaleFacSmall
                                         (isLarge, isSmall)
                                         maindata)

    doAllMono = do (s0s, s0m) <- doOne (sideGranule0ch0 side) scfsi []
                   (s1s, s1m) <- doOne (sideGranule1ch0 side) scfsi s0s
                   return $ MP3Data1Channels (headSampleRate header)
                                             (headChannels header)
                                             ((headStereoMS header, 
                                               headStereoIS header))
                                             s0m s1m

    doAllStereo = 
        do (s00s, s00m) <- doOne (sideGranule0ch0 side) scfsi0 []
           (s01s, s01m) <- doOne (sideGranule0ch1 side) scfsi1 []
           (s10s, s10m) <- doOne (sideGranule1ch0 side) scfsi0 s00s
           (s11s, s11m) <- doOne (sideGranule1ch1 side) scfsi1 s01s
           return $ MP3Data2Channels (headSampleRate header)
                                     (headChannels header)
                                     ((headStereoMS header, 
                                       headStereoIS header))
                                     s00m s01m s10m s11m

--
-- mp3Seek
--
-- Throw away bytes in the bitstream until the first byte is a frame 
-- header. This should be used to initialize the decoder before
-- calling mp3Unpack, and to reset the decoder whenever mp3Unpack
-- has found a broken frame.
--
-- Return value:
--
-- If the the bitstream is empty, or not an MP3, or really broken,
-- the function returns Nothing
-- If the bitstream is healthy, it returns a new bitstream that can
-- be used by mp3Unpack.
-- 
mp3Seek :: MP3Bitstream -> Maybe MP3Bitstream
mp3Seek (MP3Bitstream bs _) = helper bs 8192
  where
    helper bs 0    = Nothing -- Nothing interesting in 8 KB = broken.
    helper bs read = 
        case (mp3ParseFrameHeader . bsGetWord32) bs of
             Just header -> Just $ MP3Bitstream bs []
             otherwise   -> helper (B.drop 1 bs) (read - 1)

--
-- mp3UnpackFrame
--
-- Read a physical frame, and maybe return a logical frame.
--
-- A logical frame consists of a header, side data, and main data, where
-- the main data contains the scale factors and huffman data for the granules
-- as explained in mp3ParseMainData.
--
-- A physical frame looks like this: [H][C][S][data]
-- Where H is the 4 byte header, C is an optional 2 byte CRC
-- we ignore, S is the 17 (mono) or 32 (stereo) side informataion,
-- and data is a kind of buffer, that may or may not contain parts
-- of the main data we need to construct a logical frame.
--
-- In "data" in the current frame, and possibly preceding frames, are
-- the main data bits that correspond to the header/sideinfo, containing
-- the scale factors and huffman coded audio. It may look like this, 
--
-- [...][data1][data2][data3][hcs1][data3][hcs2][data3][data4][hcs3][...]
-- Where hcs1 is [H1][C1][S1].
--
-- In this example, these are physical frames:
--
-- [hcs1][data3]
-- [hcs2][data3][data4]
-- [hcs3][...] (until hcs4, not shown)
--
-- And these are the logical frames:
--
-- [hcs1][data1]
-- [hcs2][data2]
-- [hcs3][data3][data3][data3]
--
-- In the side information for a physical frame is a value known as the
-- dataptr (in the MP3 specification, this is known under a different name
-- but this name is very confusing so It's not mentioned here). 
-- If we are currently reading physical frame n, then the dataptr in physical
-- frame n+1 tells us when we have enough main data.
--
-- In the MP3Bitstream we maintain state what we've read so far. When we're
-- at the physical frame [hcs1][data3], the buffer contains
-- [data1][data2][data3]
-- We peek at the side data in [hcs2] to find out where data1 ends, 
-- copies it to the logical frame, and deletes data1 from the buffer.
-- Note that the dataptr value in [hcs2] is a negative offset from the end
-- of the buffer, not the length of data1 itself.
--
mp3UnpackFrame :: MP3Bitstream -> (MP3Bitstream, Maybe MP3LogicalFrame)
mp3UnpackFrame (MP3Bitstream bs buffer) = 
  case (mp3ParseFrameHeader . bsGetWord32) bs of
       Just header -> unpack1 header
       otherwise   -> error "Unsupported or broken header."
  where
    unpack1 header = 
      case runGet byteGetter bs of
           (Right (side, buffer', dataptr), remaining) -> 
               unpack2 header side buffer' dataptr remaining
           (otherwise, remaining)                      -> 
               (MP3Bitstream remaining [], Nothing)
      where
        byteGetter = do 
            let lengthcrc   = if headCRC header then 2 else 0
                lengthside  = if Mono == headChannels header then 17 else 32
                lengthframe = mp3FrameLength header
            head_ <- getByteString 4
            crc_  <- getByteString lengthcrc
            side  <- getByteString lengthside
            main  <- getByteString (lengthframe - 4 - lengthcrc - lengthside)
            peek  <- lookAhead $ getByteString 8
            let buffer'  = buffer ++ B.unpack main
                lbuffer' = length buffer'
            return $ (mp3ParseSideInfo header (B.unpack side), 
                      buffer', 
                      mp3PeekDataPtr peek)

    unpack2 header (Just sidedata) newbuf (Just dataptr) bs' =
      let lnewbuf            = length newbuf
          (logicalbuf, buf') = splitAt (lnewbuf - dataptr) newbuf
          -- If the state buffer is empty, the current logical frame is
          -- broken (main data that should precede the frame is missing.)
          logical = if buffer == []
                       then Nothing
                       else Just $ MP3LogicalFrame header sidedata logicalbuf
      in (MP3Bitstream bs' buf', logical)
    unpack2 _ _ _ _ rem = (MP3Bitstream rem [], Nothing)

-- 
-- mp3Unpack
--
-- A helper function that takes a bitstream, unpacks it into a logical
-- frame, then parses the main data to return an MP3Data, used by the
-- decoder.
--
mp3Unpack :: MP3Bitstream -> (MP3Bitstream, Maybe MP3Data)
mp3Unpack bitstream = helper (mp3UnpackFrame bitstream)
  where
    helper (bitstream', Nothing) = (bitstream', Nothing)
    helper (bitstream', Just logical) =
      case mp3ParseMainData logical of
           Just mdata -> (bitstream', Just mdata)
           otherwise  -> (bitstream', Nothing)

