{-# LANGUAGE BangPatterns #-}  -- Enable strictness annotations

module Main where

import qualified Data.Map.Strict as M
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import Data.Bits (shiftL, shiftR, (.|.), testBit)
import System.IO
import System.Environment (getArgs)
import Data.Word (Word8, Word16, Word32)
import Data.Char (chr, ord)
import Control.Monad (replicateM)
import qualified Data.ByteString as B 

-- 1. Data Type Definitions -----------------------------------------

-- Algebraic data type for binary digits
data Bit = Zero | One deriving (Eq, Show)

-- Huffman Tree structure using recursive algebraic data type
data HuffmanTree = Leaf Char Int      -- Character and frequency
                | Node Int HuffmanTree HuffmanTree  -- Weight and children
                deriving (Show)

-- 2. Frequency Counting --------------------------------------------

-- | Count character frequencies using strict Map
countFreq :: String -> M.Map Char Int
countFreq = foldr (\c -> M.insertWith (+) c 1) M.empty

-- 3. Priority Queue Helpers ----------------------------------------

-- | Custom priority queue implementation using sorted list
type PriorityQueue = [HuffmanTree]

-- Insert into queue while maintaining sorted order by frequency
insertSorted :: HuffmanTree -> PriorityQueue -> PriorityQueue
insertSorted x [] = [x]
insertSorted x (y:ys)
  | weight x <= weight y = x : y : ys
  | otherwise = y : insertSorted x ys
  where
    weight (Leaf _ w) = w
    weight (Node w _ _) = w

-- 4. Huffman Tree Construction -------------------------------------

-- | Build Huffman tree from frequency map
buildTree :: M.Map Char Int -> HuffmanTree
buildTree freqMap =
  let -- Convert map to list of leaves
      leaves = [Leaf c f | (c,f) <- M.toList freqMap]
      -- Build tree using priority queue
      build [] = error "Empty tree"
      build [t] = t
      build (t1:t2:rest) =
        let newWeight = weight t1 + weight t2
            newNode = Node newWeight t1 t2
         in build $ insertSorted newNode rest
      -- Recursive weight helper
      weight (Leaf _ w) = w
      weight (Node w _ _) = w
  in build $ foldr insertSorted [] leaves  -- Initial sorted queue

-- 5. Code Table Generation -----------------------------------------

-- | Generate encoding table using depth-first traversal
buildCodeTable :: HuffmanTree -> M.Map Char [Bit]
buildCodeTable tree = go [] tree M.empty
  where
    go :: [Bit] -> HuffmanTree -> M.Map Char [Bit] -> M.Map Char [Bit]
    go path (Leaf c _) m = M.insert c (reverse path) m  -- Reverse for correct order
    go path (Node _ l r) m =
      let left = go (Zero:path) l m
          right = go (One:path) r left
       in right

-- 6. Encoding Implementation ---------------------------------------

-- | Convert string to bit sequence using code table
encodeText :: M.Map Char [Bit] -> String -> [Bit]
encodeText codes = concatMap (\c -> case M.lookup c codes of
                                     Just bits -> bits
                                     Nothing -> error "Undefined character")

-- | Convert bits to bytes with padding information
bitsToBytes :: [Bit] -> ([Word8], Int)
bitsToBytes bits =
  let pad = (8 - length bits `mod` 8) `mod` 8
      padded = bits ++ replicate pad Zero
      chunkToByte chunk = foldl (\acc bit -> (acc `shiftL` 1) .|. bitValue bit) 0 chunk
      bitValue Zero = 0
      bitValue One = 1
  in (map chunkToByte (chunksOf 8 padded), pad)
  where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- 7. File Encoding -------------------------------------------------

-- | Write header containing frequency information
writeHeader :: Handle -> M.Map Char Int -> IO ()
writeHeader hdl freqMap = do
  -- Write number of entries (2 bytes)
  let count = M.size freqMap
  hPutWord16be hdl (fromIntegral count)
  -- Write each character-frequency pair
  mapM_ (\(c,f) -> do
           hPutWord8 hdl (fromIntegral (ord c))  -- Character (1 byte)
           hPutWord32be hdl (fromIntegral f)     -- Frequency (4 bytes)
        ) (M.toList freqMap)

-- | Full encoding workflow
encodeFile :: FilePath -> FilePath -> IO ()
encodeFile input output = do
  contents <- readFile input
  let freq = countFreq contents
      tree = buildTree freq
      codes = buildCodeTable tree
      bits = encodeText codes contents
      (bytes, pad) = bitsToBytes bits
  withBinaryFile output WriteMode $ \hdl -> do
    writeHeader hdl freq
    hPutWord8 hdl (fromIntegral pad)  -- Write padding information
    mapM_ (hPutWord8 hdl) bytes

-- 8. Decoding Implementation ---------------------------------------

-- | Read header and reconstruct frequency map
readHeader :: Handle -> IO (M.Map Char Int)
readHeader hdl = do
  count <- fromIntegral <$> hGetWord16be hdl
  pairs <- replicateM count $ do
    c <- chr . fromIntegral <$> hGetWord8 hdl
    f <- fromIntegral <$> hGetWord32be hdl
    return (c, f)
  return $ M.fromList pairs

-- | Convert bytes back to bits
bytesToBits :: [Word8] -> Int -> [Bit]
bytesToBits bytes pad =
  let allBits = concatMap (\b -> [if testBit b (7 - i) then One else Zero | i <- [0..7]]) bytes
  in take (length allBits - pad) allBits

-- | Decode bitstream using Huffman tree
decodeBits :: HuffmanTree -> [Bit] -> String
decodeBits tree = go tree
  where
    go _ [] = []
    go (Leaf c _) bits = c : go tree bits  -- Reset to root after leaf
    go (Node _ l r) (b:bs) =
      go (if b == Zero then l else r) bs
    go _ _ = error "Invalid bit sequence"

-- 9. File Decoding -------------------------------------------------

decodeFile :: FilePath -> FilePath -> IO ()
decodeFile input output = do
  (headerBytes, pad, dataBytes) <- withBinaryFile input ReadMode $ \hdl -> do
    freq <- readHeader hdl
    pad <- fromIntegral <$> hGetWord8 hdl
    
    -- Get file size first
    fileSize <- hFileSize hdl  -- Returns IO Integer
    -- Read all remaining bytes strictly
    dataBytes <- B.hGet hdl (fromIntegral fileSize)  -- Convert Integer to Int
    
    return (freq, pad, B.unpack dataBytes)

  let tree = buildTree headerBytes
      bits = bytesToBits dataBytes pad
      decoded = decodeBits tree bits

  writeFile output decoded

-- 10. Main Program -------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-encode", input, output] -> encodeFile input output
    ["-decode", input, output] -> decodeFile input output
    _ -> putStrLn "Usage: huffman (-encode|-decode) input output"

-- Helper functions for binary IO -----------------------------------

-- Write a single byte (Word8) to a handle
hPutWord8 :: Handle -> Word8 -> IO ()
hPutWord8 hdl w = hPutStr hdl (pure (chr (fromIntegral w)))

-- Write a 16-bit word (Word16) in big-endian format
hPutWord16be :: Handle -> Word16 -> IO ()
hPutWord16be hdl w = do
  hPutWord8 hdl (fromIntegral (w `shiftR` 8))  -- High byte
  hPutWord8 hdl (fromIntegral w)               -- Low byte

-- Write a 32-bit word (Word32) in big-endian format
hPutWord32be :: Handle -> Word32 -> IO ()
hPutWord32be hdl w = do
  hPutWord8 hdl (fromIntegral (w `shiftR` 24))  -- Byte 1 (MSB)
  hPutWord8 hdl (fromIntegral (w `shiftR` 16))  -- Byte 2
  hPutWord8 hdl (fromIntegral (w `shiftR` 8))   -- Byte 3
  hPutWord8 hdl (fromIntegral w)                -- Byte 4 (LSB)

-- Read a single byte (Word8) from a handle
hGetWord8 :: Handle -> IO Word8
hGetWord8 hdl = fromIntegral . ord <$> hGetChar hdl

-- Read a 16-bit word (Word16) in big-endian format
hGetWord16be :: Handle -> IO Word16
hGetWord16be hdl = do
  b1 <- hGetWord8 hdl
  b2 <- hGetWord8 hdl
  return $ (fromIntegral b1 `shiftL` 8) .|. fromIntegral b2

-- Read a 32-bit word (Word32) in big-endian format
hGetWord32be :: Handle -> IO Word32
hGetWord32be hdl = do
  b1 <- hGetWord8 hdl
  b2 <- hGetWord8 hdl
  b3 <- hGetWord8 hdl
  b4 <- hGetWord8 hdl
  return $ (fromIntegral b1 `shiftL` 24) .|. 
           (fromIntegral b2 `shiftL` 16) .|. 
           (fromIntegral b3 `shiftL` 8) .|. 
            fromIntegral b4
