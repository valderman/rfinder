module Codec.NBT.Get (readNBT) where
import Codec.NBT.NBTData
import Data.Binary
import Data.Binary.Get
import Data.Word
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Binary.IEEE754
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Data.Array

-- | Read an NBT value
readNBT :: Get NBTData
readNBT = getTagged >>= (\(Just (_, p)) -> return p)

-- | Read a named and tagged NBT value.
getTagged :: Get (Maybe (Name, NBTData))
getTagged = do
  t <- getWord8
  -- TAG_End has no name and no payload
  if t == 0
    then return Nothing
    else do
      (TString name) <- getString
      payload <- getUntagged t name
      return $ Just (name, payload)
  
-- | Read an unnamed, untagged NBT value; the type of the value to read is
--   expected to be of the specified type.
getUntagged :: Word8 -> Name -> Get NBTData
getUntagged 1  _ = getWord8 >>= return . TByte
getUntagged 2  _ = getWord16be >>= return . TShort
getUntagged 3  _ = getWord32be >>= return . TInt
getUntagged 4  _ = getWord64be >>= return . TLong
getUntagged 5  _ = getFloat32be >>= return . TFloat
getUntagged 6  _ = getFloat64be >>= return . TDouble
getUntagged 7  _ = getByteArray >>= return . TByteArr
getUntagged 8  _ = getString
getUntagged 9  _ = getList
getUntagged 10 n = getCompound n []
getUntagged 11 n = getIntArray >>= return . TIntArr
getUntagged t  _ = fail $ "Bad type! Type " ++ show t ++ " is unknown!"

-- | Read a fixed-length list of identically typed items.
getList :: Get NBTData
getList = do
  t <- getWord8
  len <- getWord32be
  mapM (\_ -> getUntagged t empty) [1..len] >>= return . TList

-- | Read an NBT compound value
getCompound :: Name -> [(Name, NBTData)] -> Get NBTData
getCompound name = go
  where
    go acc = do
      m <- getTagged
      case m of
        Just (n, p) -> go $ (n, p) : acc
        _           -> return $ TCompound name $ M.fromList acc

-- | Read the size of a byte array as a 32-bit big endian int, then read that
--   many bytes from the input stream.
getByteArray :: Get B.ByteString
getByteArray = getWord32be >>= getLazyByteString . fromIntegral

-- | Read the size of a byte array as a 32-bit big endian int, then read that
--   many bytes from the input stream.
getIntArray :: Get (Array Int Word32)
getIntArray = do
  elems <- getWord32be >>= return . fromIntegral
  is <- sequence $ Prelude.replicate elems getWord32be
  return $ listArray (0, elems-1) is

-- | Get an UTF-8 encoded string from the input stream and convert it to a lazy
--   Text value.
getString :: Get NBTData
getString =
  getWord16be
  >>= getLazyByteString . fromIntegral
  >>= return . TString . decodeUtf8
