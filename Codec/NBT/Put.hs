module Codec.NBT.Put (putNBT) where
import Data.Binary.Put
import Data.Binary.IEEE754
import Codec.NBT.NBTData
import qualified Data.Map as M
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Word
import Data.Array

putNBT :: NBTData -> Put
putNBT (TCompound n m) = do
  putWord8 10
  putString n
  putMap m
putNBT _ =
  fail "NBT data must have a compound root element!"

writeNBT :: NBTData -> Put
writeNBT (TByte w)       = putWord8 w
writeNBT (TShort w)      = putWord16be w
writeNBT (TInt w)        = putWord32be w
writeNBT (TLong w)       = putWord64be w
writeNBT (TFloat f)      = putFloat32be f
writeNBT (TDouble d)     = putFloat64be d
writeNBT (TByteArr bs)   = putBytes bs
writeNBT (TString txt)   = putString txt
writeNBT (TList xs)      = putList xs
writeNBT (TCompound _ m) = putMap m
writeNBT (TIntArr is)    = putInts is

-- | Write a list to the output stream. This is slow for large lists due to
--   the use of length; do something about that sometime.
putList :: [NBTData] -> Put
putList xs= do
  putWord8 (typeOfList xs)
  putWord32be $ fromIntegral $ Prelude.length xs
  mapM_ writeNBT xs

-- | Write a byte array to the output stream.
putBytes :: B.ByteString -> Put
putBytes bs = do
  putWord32be (fromIntegral $ B.length bs)
  putByteString bs

-- | Write a byte array to the output stream.
putInts :: Array Int Word32 -> Put
putInts is = do
  putWord32be (fromIntegral . snd $ bounds is)
  mapM_ (putWord32be . fromIntegral) $ elems is

-- | Write an UTF-8 encoded string to the output stream; the string must not
--   be longer than 2^16 bytes when encoded as UTF-8.
putString :: Text -> Put
putString txt = do
  putWord16be (fromIntegral $ BL.length bs)
  putLazyByteString bs
  where
    bs = encodeUtf8 txt

-- | Write a compound value to the output stream.
putMap :: M.Map Name NBTData -> Put
putMap m = do
  mapM_ putEntry $ M.toList m
  putWord8 0 -- TAG_end
  where
    putEntry (name, value) = do
      putWord8 $ typeOf value
      putString name
      writeNBT value
