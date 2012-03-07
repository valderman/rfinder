module Region (NBTRegion (..), readRegionFile) where
import qualified Data.ByteString.Lazy as B
import Data.Binary.Get
import Data.Binary
import Codec.Compression.Zlib
import Data.Array
import Data.Word
import Data.Bits
import Codec.NBT
import qualified Data.Map as M
import Data.Text.Lazy

type Coord2 = (Int, Int)

newtype NBTRegion = NBTRegion {unR :: Array Coord2 NBTData}

instance Binary NBTRegion where
  get = readRegion >>= return . NBTRegion
  put = error "Sorry, no write support! :("

-- | Read in a region file.
readRegionFile :: FilePath -> IO NBTRegion
readRegionFile fp = B.readFile fp >>= return . decode

-- | Read all chunks in a region.
readRegion :: Get (Array Coord2 NBTData)
readRegion = do
  lookAhead readLocs >>= readChunks

-- | Read all chunks at the given coordinates and return them as an array
--   indexed by said coordinates. The given list should contain 32x32 elements.
readChunks :: [(Coord2, Int)] -> Get (Array Coord2 NBTData)
readChunks locs = do
  chunks <- mapM readChunkAt locs
  return $ array ((0,0), (31,31)) chunks

-- | Read the chunk at the given position, and return it pared with its
--   position.
readChunkAt :: (Coord2, Int) -> Get (Coord2, NBTData)
readChunkAt (coords, offset) = lookAhead $ do
  -- If offset == 0, then this chunk isn't generated yet, so return an empty
  -- ByteString.
  if offset == 0
    then return (coords, TCompound empty $ M.fromList [])
    else do
      skip offset
      chunk <- readChunk
      return (coords, chunk)

-- | Reads the NBT data stored in a chunk.
readChunk :: Get NBTData
readChunk = do
  cdata <- readChunkData
  return $ decode cdata

-- | Read and decompress a chunk, then interpret the NBT data contained within.
readChunkData :: Get B.ByteString
readChunkData = do
  len <- getWord32be
  cm <- getWord8
  if cm /= 2
    then fail "File uses unsupported compression method 2!"
    else do
      chunkData <- getLazyByteString (fromIntegral len-1)
      return $ decompress chunkData

-- | Read all chunk locations, paired with their respective coordinates.
readLocs :: Get [(Coord2, Int)]
readLocs = do
  sequence [readLoc (x, z) | z <- [0..31], x <- [0..31]]

-- | Given a chunk coordinate, read the location of that chunk and return
--   it coupled with the chunk coordinates.
readLoc :: Coord2 -> Get (Coord2, Int)
readLoc (x, z) = do
  n <- getWord32be
  return ((x, z), fromIntegral $ (n `shiftR` 8) * 4096)
