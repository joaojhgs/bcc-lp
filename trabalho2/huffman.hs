{-# LANGUAGE BangPatterns #-}  -- Enable strictness annotations for performance

module Main where

-- Import necessary modules
import qualified Data.Map.Strict as M  -- Efficient key-value storage for frequencies
import qualified Data.ByteString as B  -- Strict ByteString for efficient binary I/O
import Data.List (sortBy, groupBy)     -- List utilities for sorting and grouping
import Data.Ord (comparing)            -- For comparing elements during sorting
import Data.Bits (shiftL, (.|.), testBit, shiftR)  -- Bitwise operations
import System.IO                       -- File I/O operations
import System.Environment (getArgs)    -- Command-line argument parsing
import Data.Word (Word8, Word16, Word32)  -- Fixed-width integer types for binary I/O
import Data.Char (chr, ord)            -- Character conversion utilities
import Control.Monad (replicateM)      -- Monadic utilities for repeated actions

-- 1. Data Type Definitions -----------------------------------------

-- | Binary bit representation using algebraic data types
data Bit = Zero | One deriving (Eq, Show)

-- | Huffman Tree structure using recursive algebraic data types
data HuffmanTree = Leaf Char Int      -- Leaf node: character and frequency
                | Node Int HuffmanTree HuffmanTree  -- Internal node: weight and children
                deriving (Show)

-- 2. Frequency Counting --------------------------------------------

-- | Count character frequencies using a strict Map
countFreq :: String -> M.Map Char Int
countFreq = foldr (\c -> M.insertWith (+) c 1) M.empty
-- Explanation:
-- - `foldr`: Fold the string from the right, accumulating frequencies in a Map
-- - `M.insertWith (+) c 1`: Insert character `c` into the Map, incrementing its count if it exists

-- 3. Priority Queue Helpers ----------------------------------------

-- | Custom priority queue implementation using a sorted list
type PriorityQueue = [HuffmanTree]

-- | Insert a Huffman tree into the priority queue while maintaining sorted order
insertSorted :: HuffmanTree -> PriorityQueue -> PriorityQueue
insertSorted x [] = [x]
insertSorted x (y:ys)
  | weight x <= weight y = x : y : ys
  | otherwise = y : insertSorted x ys
  where
    weight (Leaf _ w) = w
    weight (Node w _ _) = w
-- Explanation:
-- - Maintains a sorted list of Huffman trees by frequency
-- - Used during Huffman tree construction

-- 4. Huffman Tree Construction -------------------------------------

-- | Build a Huffman tree from a frequency map
buildTree :: M.Map Char Int -> HuffmanTree
buildTree freqMap =
  let -- Convert the frequency map to a list of leaf nodes
      leaves = [Leaf c f | (c,f) <- M.toList freqMap]
      -- Build the tree using a priority queue
      build [] = error "Empty tree"
      build [t] = t
      build (t1:t2:rest) =
        let newWeight = weight t1 + weight t2
            newNode = Node newWeight t1 t2
         in build $ insertSorted newNode rest
      -- Helper to extract weight from a Huffman tree
      weight (Leaf _ w) = w
      weight (Node w _ _) = w
  in build $ foldr insertSorted [] leaves
-- Explanation:
-- - `leaves`: Convert frequency map to leaf nodes
-- - `build`: Combine the two smallest nodes until one tree remains
-- - `insertSorted`: Maintains the priority queue order

-- 5. Code Table Generation -----------------------------------------

-- | Generate an encoding table using depth-first traversal
buildCodeTable :: HuffmanTree -> M.Map Char [Bit]
buildCodeTable tree = go [] tree M.empty
  where
    go :: [Bit] -> HuffmanTree -> M.Map Char [Bit] -> M.Map Char [Bit]
    go path (Leaf c _) m = M.insert c (reverse path) m  -- Store path for leaf nodes
    go path (Node _ l r) m =
      let left = go (Zero:path) l m  -- Traverse left (add Zero to path)
          right = go (One:path) r left  -- Traverse right (add One to path)
       in right
-- Explanation:
-- - `go`: Recursively traverse the tree, building paths for each character
-- - `reverse path`: Paths are built in reverse order during traversal

-- 6. Encoding Implementation ---------------------------------------

-- | Convert text to a bit sequence using the code table
encodeText :: M.Map Char [Bit] -> String -> [Bit]
encodeText codes = concatMap (\c -> case M.lookup c codes of
                                     Just bits -> bits
                                     Nothing -> error "Undefined character")
-- Explanation:
-- - `concatMap`: Map each character to its bit sequence and concatenate the results
-- - `M.lookup`: Look up the bit sequence for each character

-- | Convert bits to bytes with padding information
bitsToBytes :: [Bit] -> ([Word8], Int)
bitsToBytes bits =
  let pad = (8 - length bits `mod` 8) `mod` 8  -- Calculate padding
      padded = bits ++ replicate pad Zero  -- Add padding bits
      chunkToByte chunk = foldl (\acc bit -> (acc `shiftL` 1) .|. bitValue bit) 0 chunk
      bitValue Zero = 0
      bitValue One = 1
  in (map chunkToByte (chunksOf 8 padded), pad)
  where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)
-- Explanation:
-- - `pad`: Number of padding bits needed to make the bit sequence a multiple of 8
-- - `chunkToByte`: Convert 8 bits to a byte using bitwise operations
-- - `chunksOf`: Split the bit sequence into 8-bit chunks

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
-- Explanation:
-- - `hPutWord16be`: Write 16-bit integer in big-endian format
-- - `hPutWord32be`: Write 32-bit integer in big-endian format
-- - `mapM_`: Perform IO actions for each character-frequency pair

-- | Full encoding workflow
encodeFile :: FilePath -> FilePath -> IO ()
encodeFile input output = do
  contents <- readFile input  -- Read input file
  let freq = countFreq contents  -- Count character frequencies
      tree = buildTree freq  -- Build Huffman tree
      codes = buildCodeTable tree  -- Generate code table
      bits = encodeText codes contents  -- Encode text to bits
      (bytes, pad) = bitsToBytes bits  -- Convert bits to bytes
  withBinaryFile output WriteMode $ \hdl -> do
    writeHeader hdl freq  -- Write header
    hPutWord8 hdl (fromIntegral pad)  -- Write padding information
    mapM_ (hPutWord8 hdl) bytes  -- Write encoded bytes
-- Explanation:
-- - `withBinaryFile`: Safely open and close the output file
-- - `mapM_`: Write each byte to the file

-- 8. Decoding Implementation ---------------------------------------

-- | Read header and reconstruct frequency map
readHeader :: Handle -> IO (M.Map Char Int)
readHeader hdl = do
  count <- fromIntegral <$> hGetWord16be hdl  -- Read number of entries
  pairs <- replicateM count $ do  -- Read each character-frequency pair
    c <- chr . fromIntegral <$> hGetWord8 hdl  -- Read character
    f <- fromIntegral <$> hGetWord32be hdl  -- Read frequency
    return (c, f)
  return $ M.fromList pairs
-- Explanation:
-- - `replicateM`: Repeat the IO action `count` times
-- - `M.fromList`: Convert list of pairs to a Map

-- | Convert bytes back to bits
bytesToBits :: [Word8] -> Int -> [Bit]
bytesToBits bytes pad =
  let allBits = concatMap (\b -> [if testBit b (7 - i) then One else Zero | i <- [0..7]]) bytes
  in take (length allBits - pad) allBits
-- Explanation:
-- - `testBit`: Check if a specific bit is set in a byte
-- - `concatMap`: Convert each byte to 8 bits and concatenate the results

-- | Decode bitstream using Huffman tree
decodeBits :: HuffmanTree -> [Bit] -> String
decodeBits tree = go tree
  where
    go (Leaf c _) [] = [c]  -- Final character
    go (Leaf c _) bits = c : go tree bits  -- Reset to root after leaf
    go (Node _ l r) (b:bs) =
      go (if b == Zero then l else r) bs  -- Traverse left or right
    go _ [] = []  -- No more bits to process
-- Explanation:
-- - `go`: Recursively traverse the tree based on the bitstream
-- - `Leaf`: Found a character, reset to root for the next character

-- 9. File Decoding -------------------------------------------------

decodeFile :: FilePath -> FilePath -> IO ()
decodeFile input output = do
  (headerBytes, pad, dataBytes) <- withBinaryFile input ReadMode $ \hdl -> do
    freq <- readHeader hdl  -- Read header
    pad <- fromIntegral <$> hGetWord8 hdl  -- Read padding
    fileSize <- hFileSize hdl  -- Get file size
    dataBytes <- B.hGet hdl (fromIntegral fileSize)  -- Read all remaining bytes
    return (freq, pad, B.unpack dataBytes)  -- Convert to [Word8]

  let tree = buildTree headerBytes  -- Rebuild Huffman tree
      bits = bytesToBits dataBytes pad  -- Convert bytes to bits
      decoded = decodeBits tree bits  -- Decode bitstream

  writeFile output decoded  -- Write decoded text to output file
-- Explanation:
-- - `withBinaryFile`: Safely open and close the input file
-- - `B.hGet`: Read all remaining bytes strictly
-- - `B.unpack`: Convert ByteString to [Word8]

-- 10. Main Program -------------------------------------------------

main :: IO ()
main = do
  args <- getArgs  -- Parse command-line arguments
  case args of
    ["-encode", input, output] -> encodeFile input output  -- Encode mode
    ["-decode", input, output] -> decodeFile input output  -- Decode mode
    _ -> putStrLn "Usage: huffman (-encode|-decode) input output"  -- Invalid usage
-- Explanation:
-- - `getArgs`: Get command-line arguments
-- - `case`: Pattern match to determine mode

-- Helper functions for binary I/O -----------------------------------

-- | Write a single byte (Word8) to a handle
hPutWord8 :: Handle -> Word8 -> IO ()
hPutWord8 hdl w = hPutStr hdl (pure (chr (fromIntegral w)))

-- | Write a 16-bit word (Word16) in big-endian format
hPutWord16be :: Handle -> Word16 -> IO ()
hPutWord16be hdl w = do
  hPutWord8 hdl (fromIntegral (w `shiftR` 8))  -- High byte
  hPutWord8 hdl (fromIntegral w)               -- Low byte

-- | Write a 32-bit word (Word32) in big-endian format
hPutWord32be :: Handle -> Word32 -> IO ()
hPutWord32be hdl w = do
  hPutWord8 hdl (fromIntegral (w `shiftR` 24))  -- Byte 1 (MSB)
  hPutWord8 hdl (fromIntegral (w `shiftR` 16))  -- Byte 2
  hPutWord8 hdl (fromIntegral (w `shiftR` 8))   -- Byte 3
  hPutWord8 hdl (fromIntegral w)                -- Byte 4 (LSB)

-- | Read a single byte (Word8) from a handle
hGetWord8 :: Handle -> IO Word8
hGetWord8 hdl = fromIntegral . ord <$> hGetChar hdl

-- | Read a 16-bit word (Word16) in big-endian format
hGetWord16be :: Handle -> IO Word16
hGetWord16be hdl = do
  b1 <- hGetWord8 hdl
  b2 <- hGetWord8 hdl
  return $ (fromIntegral b1 `shiftL` 8) .|. fromIntegral b2

-- | Read a 32-bit word (Word32) in big-endian format
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