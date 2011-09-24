{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where
import qualified Data.ByteString.Lazy as B
import System.Environment (getArgs)
import Data.Array
import Data.Array.ST
import Control.Monad.ST
import Control.Monad
import Region
import Codec.NBT
import Data.Word
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import Data.List
import Data.Int

import Control.Exception
import System.IO.Unsafe
import Debug.Trace

type Coord2 = (Int64, Int64)
type Coord3 = (Int64, Int64, Int64)
type RegionArray = Array Coord2 (Array Coord2 B.ByteString)

main = do
  args <- getArgs
  if length args < 5
    then error "Usage: rfinder path x1 z1 x2 z2"
    else return ()
  let [x1, z1, x2, z2] = map read . tail $ args
  let min' = (min x1 x2, 0, min z1 z2)
  let max' = (max x1 x2, 20, max z1 z2)
  let coords = between min' max'
  regions <- readRegions (head args) (x1, z1) (x2, z2)
  
  -- The general idea here is to create a list of all coordinates to search,
  -- then check each one of them. This could be done more efficiently by just
  -- mapping over the bytestrings representing each chunk, but this seems fast
  -- enough for now, so why bother?
  let diamonds = reverse $ sort $ getGroups min' max' regions coords
  mapM_ printSite diamonds

getGroups :: Coord3 -> Coord3 -> RegionArray -> [Coord3] -> [(Int, Coord3)]
getGroups c1@(x1, y1, z1) c2@(x2, y2, z2) regs cs =
  runST $ do
    regs' <- mapM (mapM toArray) regs
    marks <- newListArray (c1, c2) True :: ST s (STUArray s Coord3 Bool)
    groups <- mapM (allAdjacent marks) cs
    return $ map (\x -> (length x, head x)) $ filter (not . null) groups
  where
    toArray = newListArray (0, 16*16*128-1) . B.unpack

    -- Mark the given coordinate as a resource, then returns all adjacent
    -- blocks that contain resources and aren't marked. Do this recursively
    -- until all consecutive resource blocks are found; return the result plus
    -- the original resource block.
    allAdjacent m c@(x, y, z) = do
      res <- isResource m c
      if res
        then do
          writeArray m c False
          rs <- filterM (isResource m)
                  $ between (x-1, y-1, z-1) (x+1, y+1, z+1)
          mapM_ (\c -> writeArray m c False) rs
          rss <- mapM (allAdjacent m) rs
          return $ c:(concat $ rs:rss)
        else
          return []

    -- Returns true if the given block is an un-marked resource block,
    -- otherwise false.
    -- At the moment, diamond (56) is hard-coded as the resource we're looking
    -- for.
    isResource m coord@(x, y, z) = do
      if x < x1 || x > x2 || y < y1 || y > y2 || z < z1 || z > z2
        then return False
        else case block coord of
               id | id == 56 -> readArray m coord
               _             -> return False

-- | Print the location of a dig site, along with the number of resources
--   to be found on that site in the following format:
--   n  (x,y,z)
--   The number of spaces separating n from (x,y,z) will always be at least one
--   and at most two.
printSite :: (Int, Coord3) -> IO ()
printSite (n, c) = putStrLn $ (pad 3 n) ++ show c
  where
    pad n x = let s = show x in s ++ replicate (max 1 (n-length s)) ' '

-- | Returns the block ID at the given coordinates. Returns 0 if the
--   coordinates point to a chunk that hasn't been loaded or generated.
getBlock :: Array Coord2 (Array Coord2 (STUArray s Int64 Word8))
         -> Coord3
         -> ST s Word8
getBlock rs (x, y, z) = block
  where
    -- Region coordinates; each region contains 32x32 chunks, which in turn
    -- contain 16x16 blocks, so there's a new region every 512 (16*32) blocks.
    (rx, rz) = (x `div` 512, z `div` 512)
    -- Chunk coordinates; each chunk consists of 16 blocks and is part of a 32x32
    -- chunk region.
    (cx, cz) = ((x `div` 16) `mod` 32, (z `div` 16) `mod` 32)
    -- Block coordinates; both regions and chunks end on multiples of 16, so
    -- the block coordinates just become indices into the 16x16-sized chunks.
    (bx, bz) = (x `mod` 16,  z `mod` 16)
    region   = rs ! (rx, rz)
    chunk    = region ! (cx, cz)
    -- Magic numbers in calculating block offsets; see
    -- http://www.minecraftwiki.net/wiki/Alpha_Level_Format/Chunk_File_Format
    -- for details on this calculation.
    block    = readArray (y+bz*128+bx*128*16)

-- | Read in all blocks in the regions in the given interval, returning an
--   array of the the regions in said interval.
readRegions :: String -> Coord2 -> Coord2 -> IO RegionArray
readRegions path (x1, z1) (x2, z2) = do
  let (rx1, rz1) = (x1 `div` 512, z1 `div` 512)
  let (rx2, rz2) = (x2 `div` 512, z2 `div` 512)
  let regions = between (rx1, 0, rz1) (rx2, 0, rz2)
  mapM (getRegion path) regions >>= return . array ((rx1, rz1), (rx2, rz2))
  where
    regFileName path (x, _, z) =
      path ++ "/region/r." ++ show x ++ "." ++ show z ++ ".mcr"
    
    getRegion p c@(x, _, z) = do
      reg <- readRegionFile (regFileName p c)
      return ((x, z), fmap blocksOnly $ unR reg)
    
    blocksOnly (TCompound _ m) =
      case m M.! T.pack "Level" of 
        TCompound _ m' ->
          case m' M.! T.pack "Blocks" of
            TBytes bs -> bs

-- | Create a list of all coordinates between (x1,y1,z1) and (x2,y2,z2).
--   Assumes all components of the first coordinate to be strictly less than
--   all components of the second coordinate.
between :: Coord3 -> Coord3 -> [Coord3]
between (x1, y1, z1) (x2, y2, z2) =
  [(x, y, z)| x <- [x1, x1+1 .. x2],
              y <- [y1, y1+1 .. y2],
              z <- [z1, z1+1 .. z2]]
