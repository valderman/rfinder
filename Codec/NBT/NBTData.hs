module Codec.NBT.NBTData (NBTData (..), Name, typeOf, typeOfList) where
import Data.Word
import Data.Text.Lazy
import Data.Map
import Data.ByteString.Lazy

type Name = Text

data NBTData
  = TByte Word8
  | TShort Word16
  | TInt Word32
  | TLong Word64
  | TFloat Float
  | TDouble Double
  | TBytes ByteString
  | TString Text
  | TList [NBTData]
  | TCompound Text (Map Name NBTData)
    deriving Show

-- | The type tag word for each NBTData value.
typeOf :: NBTData -> Word8
typeOf (TByte _)       = 1
typeOf (TShort _)      = 2
typeOf (TInt _)        = 3
typeOf (TLong _)       = 4
typeOf (TFloat _)      = 5
typeOf (TDouble _)     = 6
typeOf (TBytes _)      = 7
typeOf (TString _)     = 8
typeOf (TList _)       = 9
typeOf (TCompound _ _) = 10

-- | Safely get the type of a list; if the list is empty, byte is assumed.
--   This type defaulting for empty lists might break some badly written code,
--   so perhaps this decision should be revisited later.
typeOfList :: [NBTData] -> Word8
typeOfList (x:xs) = typeOf x
typeOfList _      = 1
