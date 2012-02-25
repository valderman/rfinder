{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
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
import Data.Maybe (fromJust)
import BlockStore

-- | The various obstacles that might be encountered when digging straight
--   down to a resource cluster. Constructors must be listed in order from
--   best to worst, so that if a is worse than b, then a > b, according to the
--   derived Ord instance.
data PathFeature = Safe | Air | Water | Lava
  deriving (Show, Eq, Ord)

main = do
  args <- getArgs
  if length args < 5
    then error "Usage: rfinder path <x1> <z1> <x2> <z2> [iron | clay | diamond | gold | <blockID>]"
    else return ()
  let [x1,z1,x2,z2] = map read . take 4 . tail $ args
      rest = drop 5 args
      -- By default, only look for diamonds. If anything else is specified,
      -- look at anything at sea level or below.
      -- what are we looking for?
      (resourceBlockID, maxY) =
        case rest of
          ["diamond"] -> (56, 20)
          ["clay"] -> (82, 64)
          ["iron"] -> (15, 64)
          ["gold"] -> (14, 32)
          [str] | [(bid, "")] <- reads str -> (bid, 64)
          _ -> (56, 20) -- diamonds by default
      min' = (min x1 x2, 0, min z1 z2)
      max' = (max x1 x2, maxY, max z1 z2)
      coords = between min' max'
  
  world <- readRegions (head args) (x1, z1) (x2, z2)
  
  -- The general idea here is to create a list of all coordinates to search,
  -- then check each one of them. This could be done more efficiently by just
  -- mapping over the bytestrings representing each chunk, but this seems fast
  -- enough for now, so why bother?
  let resources =
        reverse $ sort $ getGroups min' max' world coords resourceBlockID
  mapM_ printSite resources

-- | Get all resource clusters in the searched area.
getGroups :: Coord3
          -> Coord3
          -> World
          -> [Coord3]
          -> Word8
          -> [(Int, PathFeature, Coord3)]
getGroups c1@(x1, y1, z1) c2@(x2, y2, z2) world cs ourResource =
  runST $ do
    marks <- newArray (c1, c2) True :: ST s (STUArray s Coord3 Bool)
    getGroups marks >>= return . map summarizeCluster
  where
    summarizeCluster x =
      (length x, worstPathFeature world (head x), head x)

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
        isResource coord@(x, y, z) = do
          if x < x1 || x > x2 || y < y1 || y > y2 || z < z1 || z > z2
            then return False
          else if blockAt world coord == ourResource
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
printSite :: (Int, PathFeature, Coord3) -> IO ()
printSite (num, safe, coords) =
  putStrLn $ pad 4 num ++ pad 7 safe ++ show coords
  where
    pad n x = let s = show x in s ++ replicate (max 1 (n-length s)) ' '

-- | Returns True if the path from (x, 55, z) to (x, y, z) is clear of lava,
--   water and air; that is, if digging straight down is relatively safe.
--   Y coordinate is chosen as 55 to avoid marking anything below the ocean
--   floor as unsafe.
worstPathFeature :: World -> Coord3 -> PathFeature
worstPathFeature world (x,y,z) =
  -- If the resource is at the surface, it's quite obviously safe.
  case  map (badFeature . blockAt world) $ coords of
    [] -> Safe
    xs -> maximum xs
  where
    -- Ensure that the entire "cross" that may affect the dig path is checked.
    coords = concat [between (x-1,y,z) (x+1,55,z),
                     between (x,y,z-1) (x,55,z-1),
                     between (x-1,y,z+1) (x+1,55,z+1)]

    badFeature 0  = Air
    badFeature 8  = Water
    badFeature 9  = Water
    badFeature 10 = Lava
    badFeature 11 = Lava
    badFeature _  = Safe

-- | Read in all blocks in the regions in the given interval, returning an
--   array of the the regions in said interval.
readRegions :: String -> Coord2 -> Coord2 -> IO World
readRegions path (x1, z1) (x2, z2) = do
  let (rx1, rz1) = (x1 `div` 512, z1 `div` 512)
  let (rx2, rz2) = (x2 `div` 512, z2 `div` 512)
  let regions = between (rx1, 0, rz1) (rx2, 0, rz2)
  regionList <- mapM (getRegion path) regions 
  let w = BW $ array ((rx1,rz1),(rx2,rz2)) regionList
  return w
  where
    regFileName path (x, _, z) =
      path ++ "/region/r." ++ show x ++ "." ++ show z ++ ".mca"
    
    getRegion :: FilePath -> Coord3 -> IO (Coord2, Region)
    getRegion p c@(x, _, z) = do
      reg <- readRegionFile (regFileName p c)
      return ((x, z), BR $ fmap getChunk $ unR reg)
    
    getChunk :: NBTData -> Chunk
    getChunk (TCompound _ m) =
      case chunk of
        Just c -> c
        _      -> emptyChunk
      where
        chunk = do
          TCompound _ lvl <- M.lookup "Level" m
          TList sects <- M.lookup "Sections" lvl
          sections <- mapM getSection sects
          return $ BC $ array (0, 15) $ fillBlanksWithNothing sections

    getSection :: NBTData -> Maybe (Idx, Section)
    getSection (TCompound _ d) = do
      TByte y <- M.lookup "Y" d
      TByteArr bs <- M.lookup "Blocks" d
      return (fromIntegral y, BS bs)

    fillBlanksWithNothing :: [(Idx, Section)] -> [(Idx, Maybe Section)]
    fillBlanksWithNothing = go 0
      where
        go 16 _ =
          []
        go n (x@(ix, bs):xs)
          | ix == n   = (n, Just bs) : go (n+1) xs
          | otherwise = (n, Nothing) : go (n+1) xs
        go n [] =
          (n, Nothing) : go (n+1) []

-- | Create a list of all coordinates between (x1,y1,z1) and (x2,y2,z2).
--   Assumes all components of the first coordinate to be strictly less than
--   all components of the second coordinate.
between :: Coord3 -> Coord3 -> [Coord3]
between (x1, y1, z1) (x2, y2, z2) =
  [(x, y, z) | x <- [x1, x1+1 .. x2],
               y <- [y1, y1+1 .. y2],
               z <- [z1, z1+1 .. z2]]
