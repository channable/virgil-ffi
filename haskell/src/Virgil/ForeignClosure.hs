{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE DerivingStrategies #-}
{-# HLINT ignore "Functor law" #-}
{-# LANGUAGE DeriveAnyClass #-}
module Virgil.ForeignClosure 
( -- * Types
  ForeignClosure
  -- * Lifting and lowering user closures
  , liftCBOR
  , liftJSON
  , lowerCBOR
  , lowerJSON
  -- * Low-level closure handling 
  -- (exposed for testing)
  , liftForeignClosureToVirgilFunction
  , lowerVirgilFunctionToForeignClosure
  , ptrToVirgilFunction
  , ptrFromVirgilFunction
  -- * TODO
  , PythonFinalizer
  , registerPythonFinalizer
  , attachPythonFinalizer
)
where

import Foreign.Ptr (FunPtr, Ptr)
import Foreign.Ptr qualified as Ptr
import Data.Aeson qualified as Aeson
import Data.Function ((&))
import Data.Functor ((<&>))
import GHC.Generics ( Generic )
import Control.DeepSeq (NFData)

import Virgil.Curry qualified as Curry
import Virgil qualified
import Virgil (VirgilFunction)
import GHC.Stack ( HasCallStack, withFrozenCallStack )


import Data.IORef (IORef)
import Data.IORef qualified as IORef
import GHC.IO.Unsafe (unsafePerformIO)
import Foreign (ForeignPtr)
import qualified Foreign.ForeignPtr as ForeignPtr


-- | Turn a FunPtr that might originate from external code into a callable function
foreign import ccall "dynamic" ptrToVirgilFunction :: FunPtr VirgilFunction -> VirgilFunction

-- | Turn a function into a FunPtr that can be used in foreign code
foreign import ccall "wrapper" ptrFromVirgilFunction :: VirgilFunction -> IO (FunPtr VirgilFunction)

data ForeignClosure fun = ForeignClosure {foreignClosureAddr :: Word}
    deriving stock Show
    deriving (NFData, Generic, Aeson.FromJSON, Aeson.ToJSON)

liftJSON :: (HasCallStack, Curry.Callable fun, input ~ Curry.Params fun, IO output ~ Curry.Output fun, Curry.IsTuple input, Aeson.ToJSON input, Aeson.FromJSON output, NFData output) => ForeignClosure fun -> IO fun
liftJSON foreignClosure = withFrozenCallStack $ do
    foreignClosure
    & liftForeignClosureToVirgilFunction 
    <&> Virgil.liftJSON

liftCBOR :: (HasCallStack, Curry.Callable fun, input ~ Curry.Params fun, IO output ~ Curry.Output fun, Curry.IsTuple input, Aeson.ToJSON input, Aeson.FromJSON output, NFData output) => ForeignClosure fun -> IO fun
liftCBOR foreignClosure = withFrozenCallStack $ do
    foreignClosure
    & liftForeignClosureToVirgilFunction 
    <&> Virgil.liftCBOR

lowerJSON :: (HasCallStack, Curry.Callable fun, input ~ Curry.Params fun, IO output ~ Curry.Output fun, Curry.IsTuple input, NFData input, NFData output, Aeson.FromJSON input, Aeson.ToJSON output) => fun -> IO (ForeignClosure fun)
lowerJSON fun = withFrozenCallStack $
    fun
    & Virgil.lowerJSON
    & lowerVirgilFunctionToForeignClosure

lowerCBOR :: (HasCallStack, Curry.Callable fun, input ~ Curry.Params fun, IO output ~ Curry.Output fun, Curry.IsTuple input, NFData input, NFData output, Aeson.FromJSON input, Aeson.ToJSON output) => fun -> IO (ForeignClosure fun)
lowerCBOR fun = withFrozenCallStack $
    fun
    & Virgil.lowerCBOR
    & lowerVirgilFunctionToForeignClosure

liftForeignClosureToVirgilFunction ::  (HasCallStack, Curry.Callable fun, input ~ Curry.Params fun, IO output ~ Curry.Output fun, Aeson.ToJSON input, Aeson.FromJSON output, NFData output) => ForeignClosure fun -> IO Virgil.VirgilFunction
liftForeignClosureToVirgilFunction foreignClosure = do
    foreignPtr <- attachPythonFinalizer foreignClosure
    let fun = 
            foreignClosure
            & foreignClosureAddr
            & Ptr.WordPtr 
            & Ptr.wordPtrToPtr 
            & Ptr.castPtrToFunPtr 
            & ptrToVirgilFunction
    pure $ \inbox outbox -> do
      () <- fun inbox outbox
      ForeignPtr.touchForeignPtr foreignPtr

lowerVirgilFunctionToForeignClosure :: HasCallStack => Virgil.VirgilFunction -> IO (ForeignClosure fun)
lowerVirgilFunctionToForeignClosure fun = 
    fun
    & ptrFromVirgilFunction
    <&> Ptr.castFunPtrToPtr
    <&> Ptr.ptrToWordPtr
    <&> unWordPtr
    <&> ForeignClosure
    where
        unWordPtr (Ptr.WordPtr addr) = addr



type PythonFinalizer = FunPtr (Ptr () -> IO ())

pythonFinalizerIORef :: IORef (Maybe PythonFinalizer)
{-# NOINLINE pythonFinalizerIORef #-}
pythonFinalizerIORef = unsafePerformIO (IORef.newIORef Nothing)

registerPythonFinalizer :: PythonFinalizer -> IO ()
registerPythonFinalizer fun = do
  IORef.writeIORef pythonFinalizerIORef (Just fun)
  pure ()

attachPythonFinalizer :: ForeignClosure fun -> IO (ForeignPtr (ForeignClosure fun))
attachPythonFinalizer foreignClosure = do
  let wordPtr = foreignClosure & foreignClosureAddr & Ptr.WordPtr & Ptr.wordPtrToPtr
  maybeFinalizer <- IORef.readIORef pythonFinalizerIORef
  case maybeFinalizer of
    Just finalizer -> 
      ForeignPtr.castForeignPtr <$> ForeignPtr.newForeignPtr finalizer wordPtr
    Nothing -> do
      putStrLn $ "Warning: No pythonFinalizer is registered! Leaking memory of " <> show wordPtr
      ForeignPtr.newForeignPtr_ wordPtr
    
