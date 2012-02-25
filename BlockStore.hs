module BlockStore where
import Data.Word
import Data.Array
import Data.Int
import qualified Data.ByteString.Lazy as B

type Idx = Int64
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
blockInWorld (BW world) (x, y, z) =
  blockAt (world ! (rx, rz)) (x', y, z')
  where
    (rx, x') = x `divMod` 512
    (rz, z') = z `divMod` 512

blockInRegion :: Region -> Coord3 -> Word8
blockInRegion (BR reg) (x, y, z) =
  blockAt (reg ! (cx, cz)) (x', y, z')
  where
    (cx, x') = x `quotRem` 16
    (cz, z') = z `quotRem` 16

blockInChunk :: Chunk -> Coord3 -> Word8
blockInChunk (BC sections) (x, y, z) =
  case sections ! sy of
    Just bs -> blockAt bs (x, y', z)
    _       -> 0
  where
    (sy, y') = y `quotRem` 16

blockInSect :: Section -> Coord3 -> Word8
blockInSect (BS bs) (x, y, z) = bs `B.index` (x + z*16 + y*256)
