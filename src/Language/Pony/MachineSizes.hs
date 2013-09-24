-- The following file is automatically generated. Do not edit it.

module Language.Pony.MachineSizes where
sizeOfChar :: Int
sizeOfChar = 8

sizeOfShort :: Int
sizeOfShort = 16

sizeOfInt :: Int
sizeOfInt = 32

sizeOfLong :: Int
sizeOfLong = 64

sizeOfLongLong :: Int
sizeOfLongLong = 64

sizeOfInt128 :: Int
sizeOfInt128 = 128

sizeOfFloat :: Int
sizeOfFloat = 32

sizeOfDouble :: Int
sizeOfDouble = 64

sizeOfLongDouble :: Int
sizeOfLongDouble = 128

intTypeFromSize :: Int -> String
intTypeFromSize 16 = "short"
intTypeFromSize 32 = "int"
intTypeFromSize 64 = "long"
intTypeFromSize _ = error "bad size"