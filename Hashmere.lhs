> module Hashmere where
> import Char (ord)
> import Data.Int		( Int32, Int64 )
> import Data.List
> import Data.Bits

The next few functions are copied from Data.HashTable.hs. One day I might
fix the code that calls this to use the "standard" version of this. For now,
at least it all compiles.

> golden :: Int32
> golden = -1640531527

A sample (and useful) hash function for Int and Int32,
implemented by extracting the uppermost 32 bits of the 64-bit
result of multiplying by a 32-bit constant.  The constant is from
Knuth, derived from the golden ratio:

golden = round ((sqrt 5 - 1) * 2^31) :: Int


> hashInt :: Int -> Int32
> hashInt x = mulHi (fromIntegral x) golden

> -- hi 32 bits of a x-bit * 32 bit -> 64-bit multiply
> mulHi :: Int32 -> Int32 -> Int32
> mulHi a b = fromIntegral (r `shiftR` 32)
>  where r :: Int64
>        r = fromIntegral a * fromIntegral b :: Int64

A sample hash function for Strings.  We keep multiplying by the
golden ratio and adding. 
Note that this has not been extensively tested for reasonability,
but Knuth argues that repeated multiplication by the golden ratio
will minimize gaps in the hash space.

I added the test to force things to be positive, just to make things
easier for me later.

> hashString :: String -> Int32
> hashString str
>  | hashcalc < 0  = -hashcalc
>  | otherwise = hashcalc
>  where f m c = fromIntegral (ord c + 1) * golden + mulHi m golden
>        hashcalc = foldl' f 0 str