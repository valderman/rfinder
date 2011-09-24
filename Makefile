HSFILES=Main.hs Region.hs Codec/NBT/NBTData.hs Codec/NBT.hs Codec/NBT/Put.hs Codec/NBT/Get.hs

HSFLAGS=--make
OPTFLAGS=-O2 -fforce-recomp
DBGFLAGS=-prof -auto-all -rtsopts

rfinder: $(HSFILES)
	ghc $(HSFLAGS) -o rfinder Main.hs

release: $(HSFILES)
	ghc $(HSFLAGS) $(OPTFLAGS) -o rfinder Main.hs

debug: $(HSFILES)
	ghc $(HSFLAGS) $(DBGFLAGS) -o rfinder Main.hs
