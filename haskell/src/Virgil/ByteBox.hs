{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

{- | A wrapper of the common idiom 
of passing a bytestring together with its length 
back-and-forth over a foreign function interface boundary.

This module is relatively low-level. This means:
- To ensure easier reasoning about sequencing, nearly all functions use IO
- Almost all functions come with safety invariants that the caller should uphold; these are not specifically prefixed with `unsafe`.
-}
module Virgil.ByteBox where

import Foreign.Ptr ( Ptr, castPtr, plusPtr )
import Foreign.C (CStringLen, CString, CSize)
import qualified Foreign.Marshal
import Foreign.Storable (sizeOf, peek, poke, alignment)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as ByteString.Unsafe

newtype ByteBox = ByteBox (Ptr CStringLen)


-- Runs the given action, passing it a newly-built (empty!) ByteBox.
--
-- This is a very low-level function, prefer using the `with*` style functions
-- which are high-level wrappers around this.
alloca :: (ByteBox -> IO a) -> IO a
alloca action = Foreign.Marshal.allocaBytesAligned sizeOf' alignment' $ \ptr -> action (ByteBox ptr)
  where
     sizeOf' = sizeOf (undefined :: CString) + sizeOf (undefined :: CSize)
     alignment' = max (alignment (undefined :: CString)) (alignment (undefined :: CSize))


-- | Passes the ByteString to the function as a ByteBox. O(1)
--
-- NOTE: The function should not alter the ByteBox,
-- as changes would show up in the original bytestring.
withByteStringAsByteBox :: ByteString -> (ByteBox -> IO a) -> IO a
withByteStringAsByteBox bs action =
  ByteString.Unsafe.unsafeUseAsCStringLen bs $ \cl ->
    withCStringLenAsByteBox cl action

-- | Passes the CStringLen to the function as a ByteBox. O(1)
--
-- Does _no_ cleanup of the internal string buffer once the function returns.
withCStringLenAsByteBox :: CStringLen -> (ByteBox -> IO a) -> IO a
withCStringLenAsByteBox cstringlen action =
  alloca $ \bb -> do
    pokeFromCStringLen bb cstringlen
    action bb

withBorrowingByteString :: ByteBox -> (ByteString -> IO a) -> IO a
withBorrowingByteString bb action = peekToBorrowingByteString bb >>= action

-- O(1). The resulting ByteString shares the underlying buffer.
--
-- The ByteString 'borrows' the buffer, it has no finalizer registered,
-- so:
-- - the caller has to clean up the ByteBox
-- - but ensure that the last ByteString referencing it has gone out of scope beforehand!
--
-- Unsafe: If the original buffer were to be changed after this function returns, 
-- this would show up in the ByteString
-- which would break referential transparency
peekToBorrowingByteString :: ByteBox -> IO ByteString
peekToBorrowingByteString bb = do
  cstringlen <- peekToBorrowingCStringLen bb
  ByteString.Unsafe.unsafePackCStringLen cstringlen

-- | Low-level conversion between a CStringLen and a ByteBox
-- Both objects will point to the same underlying buffer
peekToBorrowingCStringLen :: ByteBox -> IO CStringLen
peekToBorrowingCStringLen (ByteBox ptr) = do
  strPtr <- peek (castPtr ptr)
  len <- peek (castPtr ptr `plusPtr` sizeOf (undefined ::CString))
  pure (strPtr, len)

-- | Low-level conversion to write an (owned) ByteString into a (borrowed) ByteBox
-- O(n), has to copy the internal bytes.
--
-- The resulting ByteBox' string buffer should be freed with `free` when no longer in use.
pokeFromByteString :: ByteBox -> ByteString -> IO ()
pokeFromByteString outBox bs = do
  ByteString.Unsafe.unsafeUseAsCStringLen bs $ \(inPtr, len) -> do
    strPtr <- Foreign.Marshal.mallocBytes len
    Foreign.Marshal.copyBytes strPtr inPtr len
    pokeFromCStringLen outBox (strPtr, len)

-- | Low-level conversion between a ByteBox and a CStringLen
-- Both objects will point to the same underlying buffer
pokeFromCStringLen :: ByteBox -> CStringLen -> IO ()
pokeFromCStringLen (ByteBox ptr) (str, len) = do
  poke (castPtr ptr) str
  poke (castPtr ptr `plusPtr` sizeOf (undefined :: CString)) len
