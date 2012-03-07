{-# LANGUAGE BangPatterns #-}
module BlockStore where
import Data.Word
import Data.Array
import Data.Bits
import qualified Data.ByteString as B

type Idx = Int
type Coord2 = (Idx, Idx)
type Coord3 = (Idx, Idx, Idx)

newtype Section = BS B.ByteString deriving Show
newtype Chunk = BC (Array Idx (Maybe Section)) deriving Show
newtype Region = BR (Array Coord2 Chunk) deriving Show
newtype World = BW (Array Coord2 Region) deriving Show

emptyChunk :: Chunk
emptyChunk =
  BC $ listArray (0, 15) $ repeat Nothing

class BlockStore a where
  blockAt :: a -> Coord3 -> Word8

instance BlockStore World where
  blockAt = blockInWorld

instance BlockStore Region where
  blockAt = blockInRegion

instance BlockStore Chunk where
  blockAt = blockInChunk

instance BlockStore Section where
  blockAt = blockInSect

blockInWorld :: World -> Coord3 -> Word8
blockInWorld (BW world) (!x, !y, !z) =
  blockAt (world ! (rx, rz)) (x', y, z')
  where
    rx = x `shiftR` 9
    rz = z `shiftR` 9
    x' = x .&. 511
    z' = z .&. 511

blockInRegion :: Region -> Coord3 -> Word8
blockInRegion (BR reg) (!x, !y, !z) =
  blockAt (reg ! (cx, cz)) (x', y, z')
  where
    cx = x `shiftR` 4
    cz = z `shiftR` 4
    x' = x .&. 15
    z' = z .&. 15

blockInChunk :: Chunk -> Coord3 -> Word8
blockInChunk (BC sections) (!x, !y, !z) =
  case sections ! sy of
    Just bs -> blockAt bs (x, y', z)
    _       -> 0
  where
    sy = y `shiftR` 4
    y' = y .&. 15

blockInSect :: Section -> Coord3 -> Word8
blockInSect (BS bs) (!x, !y, !z) =
  bs `B.index` (x + z*16 + y*256)
