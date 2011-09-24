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

type Coord2 = (Int64, Int64)
type Coord3 = (Int64, Int64, Int64)
type RegionList = Array Coord2 (Array Coord2 B.ByteString)

main = do
  args <- getArgs
  if length args < 5
    then error "Usage: rfinder path x1 z1 x2 z2"
    else return ()
  let [x1, z1, x2, z2] = map read . tail $ args
  let min' = (min x1 x2, 0, min z1 z2)
  let max' = (max x1 x2, 20, max z1 z2)
  let coords = between min' max'
  block <- readRegions (head args) (x1, z1) (x2, z2) >>= return . getBlock
  
  -- The general idea here is to create a list of all coordinates to search,
  -- then check each one of them. This could be done more efficiently by just
  -- mapping over the bytestrings representing each chunk, but this seems fast
  -- enough for now, so why bother?
  let diamonds = reverse $ sort $ getGroups min' max' block coords
  mapM_ printSite diamonds

-- | Get all diamond clusters in the searched area.
getGroups :: Coord3
          -> Coord3
          -> (Coord3 -> Word8)
          -> [Coord3]
          -> [(Int, Bool, Coord3)]
getGroups c1@(x1, y1, z1) c2@(x2, y2, z2) block cs =
  runST $ do
    marks <- newArray (c1, c2) True :: ST s (STUArray s Coord3 Bool)
    groups <- getGroups marks
    return $ map (\x -> (length x, pathIsSafe block (head x), head x)) groups
  where
    getGroups marks =
      go [] $ between c1 c2
      where
        -- Accumulate a list of all encountered groups of resources.
        go acc (c:cs) = do
          resource <- isResource c
          if resource
            then do
              mark c
              group <- floodfill c
              go ((c:group):acc) cs
            else
              go acc cs
        go acc _ =
          return acc

        -- Simple floodfill algorithm; given the coordinate of a confirmed
        -- resource, mark and add to a list all adjacent resources, then
        -- recursively apply the floodfill to them too.
        floodfill c@(x, y, z) = do
          let adjacent = between (x-1,y-1,z-1) (x+1,y+1,z+1)
          resources <- filterM isResource adjacent
          rss <- mapM floodfill resources
          return $ concat (resources:rss)

        -- Returns true if the given block is an un-marked resource block,
        -- otherwise false. After checking the block, if it was a resource,
        -- mark it as visited.
        -- At the moment, diamond (56) is hard-coded as the resource we're
        -- looking for.
        isResource coord@(x, y, z) = do
          if x < x1 || x > x2 || y < y1 || y > y2 || z < z1 || z > z2
            then return False
          else if block coord == 56
            then do
              x <- haventSeen coord
              mark coord
              return x
          else return False
        
        mark c = writeArray marks c False
        haventSeen = readArray marks

-- | Print the location of a dig site, along with the number of resources
--   to be found on that site in the following format:
--   n  safe  (x,y,z)
--   The number of spaces separating n from (x,y,z) will always be at least one
--   and at most two.
printSite :: (Int, Bool, Coord3) -> IO ()
printSite (num, safe, coords) = putStrLn $
    (pad 3 $ show num) ++
    (pad 7 $ if safe then "safe" else "unsafe") ++
    (show coords)
  where
    pad n s = s ++ replicate (max 1 (n-length s)) ' '

-- | Returns the block ID at the given coordinates. Returns 0 if the
--   coordinates point to a chunk that hasn't been loaded or generated.
--   As for the magic numbers in calculating block offsets, see
--   http://www.minecraftwiki.net/wiki/Alpha_Level_Format/Chunk_File_Format
--   for details.
getBlock :: RegionList -> Coord3 -> Word8
getBlock regions (x, y, z) = chunk `B.index` pos
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

-- | Returns True if the path from (x, 55, z) to (x, y, z) is clear of lava,
--   water and air; that is, if digging straight down is relatively safe.
pathIsSafe :: (Coord3 -> Word8) -> Coord3 -> Bool
pathIsSafe block (x,y,z) =
  all (safe . block) $ between (x,y,z) (x,55,z)
  where
    safe 0                     = False -- air; a cave or something
    safe n | n >= 8 && n <= 11 = False -- water or lava
    safe _                     = True  -- anything else

-- | Read in all blocks in the regions in the given interval, returning an
--   array of the the regions in said interval.
readRegions :: String -> Coord2 -> Coord2 -> IO RegionList
readRegions path (x1, z1) (x2, z2) = do
  let (rx1, rz1) = (x1 `div` 512, z1 `div` 512)
  let (rx2, rz2) = (x2 `div` 512, z2 `div` 512)
  let regions = between (rx1, 0, rz1) (rx2, 0, rz2)
  mapM (getRegion path) regions >>= return . array ((rx1,rz1),(rx2,rz2))
  where
    regFileName path (x, _, z) =
      path ++ "/region/r." ++ show x ++ "." ++ show z ++ ".mcr"
    
    getRegion p c@(x, _, z) = do
      reg <- readRegionFile (regFileName p c)
      return ((x, z), fmap blocksOnly $ unR reg)
    
    blocksOnly (TCompound _ m) =
      case M.lookup (T.pack "Level") m of
        Just (TCompound _ m') ->
          case m' M.! T.pack "Blocks" of
            TBytes bs -> bs
        _ ->
          error "OH SHI-"

-- | Create a list of all coordinates between (x1,y1,z1) and (x2,y2,z2).
--   Assumes all components of the first coordinate to be strictly less than
--   all components of the second coordinate.
between :: Coord3 -> Coord3 -> [Coord3]
between (x1, y1, z1) (x2, y2, z2) =
  [(x, y, z) | x <- [x1, x1+1 .. x2],
               y <- [y1, y1+1 .. y2],
               z <- [z1, z1+1 .. z2]]
