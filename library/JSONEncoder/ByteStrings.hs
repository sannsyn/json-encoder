module JSONEncoder.ByteStrings where

import JSONEncoder.Prelude
import Foreign
import qualified Data.ByteString.Internal as C


{-# INLINE utf8Char #-}
utf8Char :: Char -> ByteString
utf8Char =
  utf8Ord . ord

{-# INLINE utf8Ord #-}
utf8Ord :: Int -> ByteString
utf8Ord x =
  if
    | x <= 0x7F ->
      bytes1 (fromIntegral x)
    | x <= 0x07FF ->
      bytes2 (fromIntegral ((x `shiftR` 6) + 0xC0))
             (fromIntegral ((x .&. 0x3F) + 0x80))
    | x <= 0xFFFF ->
      bytes3 (fromIntegral (x `shiftR` 12) + 0xE0)
             (fromIntegral ((x `shiftR` 6) .&. 0x3F) + 0x80)
             (fromIntegral (x .&. 0x3F) + 0x80)
    | otherwise ->
      bytes4 (fromIntegral (x `shiftR` 18) + 0xF0)
             (fromIntegral ((x `shiftR` 12) .&. 0x3F) + 0x80)
             (fromIntegral ((x `shiftR` 6) .&. 0x3F) + 0x80)
             (fromIntegral (x .&. 0x3F) + 0x80)

{-# INLINE bytes1 #-}
bytes1 :: Word8 -> ByteString
bytes1 byte1 =
  C.unsafeCreate 1 $ \ptr -> do
    pokeByteOff ptr 0 byte1

{-# INLINE bytes2 #-}
bytes2 :: Word8 -> Word8 -> ByteString
bytes2 byte1 byte2 =
  C.unsafeCreate 2 $ \ptr -> do
    pokeByteOff ptr 0 byte1
    pokeByteOff ptr 1 byte2

{-# INLINE bytes3 #-}
bytes3 :: Word8 -> Word8 -> Word8 -> ByteString
bytes3 byte1 byte2 byte3 =
  C.unsafeCreate 3 $ \ptr -> do
    pokeByteOff ptr 0 byte1
    pokeByteOff ptr 1 byte2
    pokeByteOff ptr 2 byte3

{-# INLINE bytes4 #-}
bytes4 :: Word8 -> Word8 -> Word8 -> Word8 -> ByteString
bytes4 byte1 byte2 byte3 byte4 =
  C.unsafeCreate 4 $ \ptr -> do
    pokeByteOff ptr 0 byte1
    pokeByteOff ptr 1 byte2
    pokeByteOff ptr 2 byte3
    pokeByteOff ptr 3 byte4
