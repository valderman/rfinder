module Codec.NBT (NBTData (..)) where
import Data.Binary
import Codec.NBT.Put
import Codec.NBT.Get
import Codec.NBT.NBTData

instance Binary NBTData where
  put = putNBT
  get = readNBT
