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

type Coord2 = (Int, Int)
type Coord3 = (Int, Int, Int)
type RegionList = [(Coord2, [(Coord2, [Word8])])]

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

getGroups :: Coord3 -> Coord3 -> RegionList -> [Coord3] -> [(Int, Coord3)]
getGroups c1@(x1, y1, z1) c2@(x2, y2, z2) regs cs =
  runST $ do
    groups <- toRegionArray regs >>= getGroups
    return $ map (\x -> (length x, head x)) $ filter (not . null) groups
  where
    bounds = ((x1 `div` 512, z1 `div` 512), (x2 `div` 512, z2 `div` 512))
    toRegionArray inner = do
      arr <- mapM toChunkArray inner
      return $ array bounds arr

    toChunkArray (c, inner) = do
      arr <- mapM toBlockArray inner
      return (c, array ((0,0), (31,31)) arr)

    toBlockArray :: (Coord2, [Word8]) -> ST s (Coord2, STUArray s Int Word8)
    toBlockArray (c, bs) = do
      arr <- newListArray (0, 16*16*128 - 1) bs
      return (c, arr)

    getGroups regions =
      go [] $ between c1 c2
      where
        go acc (c:cs) = do
          resource <- isResource regions c
          if resource
            then do
              group <- floodfill c
              go ((c:group):acc) cs
            else
              go acc cs
        go acc _ =
          return acc

        floodfill (x, y, z) = do
          let adjacent = between (x-1,y-1,z-1) (x+1,y+1,z+1)
          resources <- filterM (isResource regions) adjacent
          rss <- mapM floodfill resources
          return $ concat (resources:rss)

    -- Returns true if the given block is an un-marked resource block,
    -- otherwise false.
    -- At the moment, diamond (56) is hard-coded as the resource we're looking
    -- for.
    isResource m coord@(x, y, z) = do
      if x < x1 || x > x2 || y < y1 || y > y2 || z < z1 || z > z2
        then return False
        else liftM (== 56) $ checkBlock m coord

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
--   After the block has been checked, set its ID to 1, to mark the resource
--   as already checked and thus no longer existent.
--   As for the magic numbers in calculating block offsets, see
--   http://www.minecraftwiki.net/wiki/Alpha_Level_Format/Chunk_File_Format
--   for details.
checkBlock :: Array Coord2 (Array Coord2 (STUArray s Int Word8))
           -> Coord3
           -> ST s Word8
checkBlock regions (x, y, z) = do
  id <- readArray chunk pos
  writeArray chunk pos 1 -- Mark resource as taken by changing it to stone
  return id
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
    chunk    = regions ! (rx, rz) ! (cx, cz)
    pos      = (y+bz*128+bx*128*16)

-- | Read in all blocks in the regions in the given interval, returning an
--   array of the the regions in said interval.
readRegions :: String -> Coord2 -> Coord2 -> IO RegionList
readRegions path (x1, z1) (x2, z2) = do
  let (rx1, rz1) = (x1 `div` 512, z1 `div` 512)
  let (rx2, rz2) = (x2 `div` 512, z2 `div` 512)
  let regions = between (rx1, 0, rz1) (rx2, 0, rz2)
  mapM (getRegion path) regions
  where
    regFileName path (x, _, z) =
      path ++ "/region/r." ++ show x ++ "." ++ show z ++ ".mcr"
    
    getRegion p c@(x, _, z) = do
      reg <- readRegionFile (regFileName p c)
      return ((x, z), map blocksOnly $ unR reg)
    
    blocksOnly (c, TCompound _ m) =
      case M.lookup (T.pack "Level") m of
        Just (TCompound _ m') ->
          case m' M.! T.pack "Blocks" of
            TBytes bs -> (c, B.unpack bs)
        _ ->
          (c, repeat 0) -- chunk not loaded; infinite zeroes!

-- | Create a list of all coordinates between (x1,y1,z1) and (x2,y2,z2).
--   Assumes all components of the first coordinate to be strictly less than
--   all components of the second coordinate.
between :: Coord3 -> Coord3 -> [Coord3]
between (x1, y1, z1) (x2, y2, z2) =
  [(x, y, z) | x <- [x1, x1+1 .. x2],
               y <- [y1, y1+1 .. y2],
               z <- [z1, z1+1 .. z2]]
