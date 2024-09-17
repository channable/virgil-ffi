{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- | | Actual module that is compiled into a shared library.
-- A thin (as thin as possible) wrapper around the main `Virgil` module.
module Example where
import Virgil (VirgilFunction, ByteBox(..), Ptr)
import qualified Virgil
import Virgil.ForeignClosure (ForeignClosure, PythonFinalizer)
import Virgil.ForeignClosure qualified as ForeignClosure

import GHC.Stack ( HasCallStack )
import Data.Aeson qualified as Aeson
import Foreign.Ptr qualified as Ptr
import Data.Function ((&))

-- | Exported to allow the foreign (in this case Python) code
-- to allocate, reallocate and free memory
-- in exactly the same way as Haskell
-- and therefore allocations made by one language can be cleaned up by the other.
foreign export ccall virgilRealloc :: Ptr a -> Int -> IO (Ptr a)
virgilRealloc :: Ptr a -> Int -> IO (Ptr a)
virgilRealloc = Virgil.virgilRealloc

foreign export ccall virgilRegisterPythonFinalizer :: PythonFinalizer -> IO ()
virgilRegisterPythonFinalizer :: PythonFinalizer -> IO ()
virgilRegisterPythonFinalizer = ForeignClosure.registerPythonFinalizer

foreign export ccall virgilFinalizeHaskellClosure :: Word -> IO ()
virgilFinalizeHaskellClosure :: Word -> IO ()
virgilFinalizeHaskellClosure addr =
  addr
  & Ptr.WordPtr 
  & Ptr.wordPtrToPtr 
  & Ptr.castPtrToFunPtr 
  & Ptr.freeHaskellFunPtr

-- Examples:

-- | Given a string, append some exclamation marks
foreign export ccall exclaim :: VirgilFunction
exclaim :: HasCallStack => VirgilFunction
exclaim = Virgil.lowerCBOR impl
  where
    impl :: String -> IO String
    impl input = pure $ input <> "!!!"

-- | Sum two integers
foreign export ccall sumIntegers :: VirgilFunction
sumIntegers :: HasCallStack => VirgilFunction
sumIntegers = Virgil.lowerCBOR impl
  where
    impl :: HasCallStack => [Integer] -> IO Integer
    impl ints = pure $ sum ints

-- | Divide two integers
foreign export ccall divIntegers :: VirgilFunction
divIntegers :: HasCallStack => VirgilFunction
divIntegers = Virgil.lowerCBOR impl
  where
    impl :: HasCallStack => Integer -> Integer -> IO Integer
    impl left right = Virgil.checkpointStrict $ pure $ div left right

-- | Call a Python function on each element in an list, returning that list
foreign export ccall mappy :: VirgilFunction
mappy :: HasCallStack => VirgilFunction
mappy = Virgil.wrapDebug $ Virgil.lowerCBOR impl
  where
    impl :: HasCallStack => [Aeson.Value] -> ForeignClosure (Aeson.Value -> IO Aeson.Value) -> IO [Aeson.Value]
    impl list callback = do
      putStrLn "Building fun"
      fun <- ForeignClosure.liftCBOR callback
      putStrLn "Finished building fun; calling map"
      result <- mapM fun list
      putStrLn "Finished calling map; returning"
      pure result
