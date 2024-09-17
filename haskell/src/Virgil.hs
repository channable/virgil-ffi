{-# LANGUAGE DerivingStrategies #-}
module Virgil
  ( -- * Types:
    VirgilFunction
  -- NOTE: ByteBox (and its constructor) 
  -- are re-exported as contents need to be visible to allow usage 
  -- of VirgilFunction in `foreign export`s
  , ByteBox(ByteBox) 
  , Ptr
  -- * Foundational function that needs to be re-exported
  , virgilRealloc
  -- * Lifting and lowering user functions
  , liftCBOR
  , liftJSON
  , lowerCBOR
  , lowerJSON
  -- * Exception handling helper
  , checkpointStrict
  -- * Debugging helpers
  , wrapDebug
  -- * Smaller-granularity lifting/lowering functions 
  -- These are exported for easier testing.
  -- ** Level 1<->2: ByteBox <-> ByteString
  , liftVirgilFunToBytestringFun
  , lowerBytestringFunToVirgilFun
  -- ** Level 2<->3: ByteString <-> Value
  , liftBytestringFunToJSONFun
  , liftBytestringFunToCBORFun
  , lowerJSONFunToBytestringFun
  , lowerCBORFunToBytestringFun
  -- ** Level 3<->4: Value <-> Exceptions
  , liftValueFunToThrowingFun
  , lowerThrowingFunToValueFun
) where

import Data.Bifunctor qualified as Bifunctor
import Data.Function ((&))
import Data.Functor ((<&>))
import Foreign.Ptr ( Ptr , FunPtr)
import Foreign.Marshal qualified

import Codec.CBOR.JSON qualified as CBOR.JSON
import Codec.CBOR.Write qualified as CBOR.Write
import Codec.CBOR.Read qualified as CBOR.Read
import Control.DeepSeq (NFData)
import Control.Exception.Annotated.UnliftIO
    ( Exception, checkpointCallStack, throw, MonadUnliftIO )
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as ByteString.Lazy
import GHC.Stack ( HasCallStack, withFrozenCallStack )
import UnliftIO.Exception (evaluateDeep, handle)

import Virgil.ByteBox (ByteBox(..))
import qualified Virgil.ByteBox as ByteBox
import Virgil.SerializableException (SerializableException)
import qualified Virgil.SerializableException as SerializableException
import Virgil.Curry qualified as Curry

-- | At the lowest level, functions read from one bytebox, write to a second bytebox (an 'output parameter'), and return nothing.
--
-- An 'output parameter' is chosen over returning a complex structure 
-- because Haskell's GHC and Python's ctypes have better support for mutating complex structures behind pointers rather than returning them.
type VirgilFunction = (ByteBox -> ByteBox -> IO ())

-- | Should be be re-exported by any user library, 
-- so the foreign (in this case Python) code
-- can allocate, reallocate and free memory
-- in exactly the same way as Haskell
-- and therefore allocations made by one language can be cleaned up by the other.
virgilRealloc :: Ptr a -> Int -> IO (Ptr a)
virgilRealloc ptr size = Foreign.Marshal.reallocBytes ptr size



liftVirgilFunToBytestringFun :: HasCallStack => VirgilFunction -> (ByteString -> IO ByteString)
liftVirgilFunToBytestringFun fun = \inStr ->
    ByteBox.withByteStringAsByteBox inStr $ \inBox ->
        ByteBox.alloca $ \outBox -> do
            () <- fun inBox outBox
            ByteBox.peekToBorrowingByteString outBox

lowerBytestringFunToVirgilFun :: HasCallStack => (ByteString -> IO ByteString) -> VirgilFunction
lowerBytestringFunToVirgilFun fun = \ inBox outBox -> ByteBox.withBorrowingByteString inBox fun >>= ByteBox.pokeFromByteString outBox

-- | Thrown when the parameter(s) passed from foreign land to a Haskell function could not be parsed
newtype InputParseException = InputParseException String
  deriving newtype Show
  deriving NFData

instance Exception InputParseException

-- | Thrown when the return result of calling a foreign function could not be parsed
newtype OutputParseException = OutputParseException String
  deriving newtype Show
  deriving NFData

instance Exception OutputParseException

liftBytestringFunToJSONFun :: (HasCallStack, Curry.IsTuple input, Aeson.ToJSON input, Aeson.FromJSON output) => (ByteString -> IO ByteString) -> (input -> IO (Either OutputParseException output))
liftBytestringFunToJSONFun fun = \input ->
  input & encode & fun <&> decode
    where
      decode = Bifunctor.first OutputParseException . Aeson.eitherDecodeStrict
      encode = ByteString.Lazy.toStrict . Aeson.encode

lowerJSONFunToBytestringFun :: (HasCallStack, Curry.IsTuple input, Aeson.FromJSON input, Aeson.ToJSON output) => (Either InputParseException input -> IO output) -> (ByteString -> IO ByteString)
lowerJSONFunToBytestringFun fun = \input ->
  input & decode & fun <&> encode
    where
      decode = Bifunctor.first InputParseException . Aeson.eitherDecodeStrict
      encode = ByteString.Lazy.toStrict . Aeson.encode

liftBytestringFunToCBORFun :: (HasCallStack, Curry.IsTuple input, Aeson.ToJSON input, Aeson.FromJSON output) => (ByteString -> IO ByteString) -> (input -> IO (Either OutputParseException output))
liftBytestringFunToCBORFun fun = \input ->
  input & cborEncode & fun <&> cborDecode OutputParseException

lowerCBORFunToBytestringFun :: (HasCallStack, Curry.IsTuple input, Aeson.FromJSON input, Aeson.ToJSON output) => (Either InputParseException input -> IO output) -> (ByteString -> IO ByteString)
lowerCBORFunToBytestringFun fun = \input ->
  input & cborDecode InputParseException & fun <&> cborEncode

cborDecode :: Aeson.FromJSON b => (String -> exception) -> ByteString -> Either exception b
cborDecode exception bytestring = do
        (rest, aesonValue) <- Bifunctor.first (exception . show) $ CBOR.Read.deserialiseFromBytes (CBOR.JSON.decodeValue True) $ ByteString.Lazy.fromStrict bytestring
        fullValue <- Bifunctor.first exception $ Aeson.Types.parseEither Aeson.parseJSON aesonValue
        if ByteString.Lazy.length rest /= 0 then
          Left $ exception ( "Expected to use full bytestring, but leftover data found:" <> show rest)
        else
          pure fullValue

cborEncode :: Aeson.ToJSON output => output -> ByteString
cborEncode = CBOR.Write.toStrictByteString . CBOR.JSON.encodeValue . Aeson.toJSON

-- | Wrap a VirgilFunction to debug its input and output serialization bytes.
--
-- Intended only for debugging, as it adds some overhead.
wrapDebug :: VirgilFunction -> VirgilFunction
wrapDebug fun = lowerBytestringFunToVirgilFun $ \input -> do
  putStr "input: "
  print input
  output <- liftVirgilFunToBytestringFun fun input
  putStr "output: "
  print output
  pure output

liftValueFunToThrowingFun
  :: (HasCallStack, NFData output, Aeson.ToJSON input, Aeson.FromJSON output)
  => (input -> IO (Either OutputParseException (Either SerializableException output)))
  -> (input -> IO output)
liftValueFunToThrowingFun fun = \input -> do
  output <- fun input
  case output of
    Left ex -> throw ex -- parsing callback output failed
    Right (Left ex) -> withFrozenCallStack $ SerializableException.rethrowPythonException ex -- Running callback failed
    Right (Right val) -> pure val

lowerThrowingFunToValueFun
  :: (HasCallStack, NFData input, Aeson.FromJSON input, NFData output, Aeson.ToJSON output)
  => (input -> IO output)
  -> (Either InputParseException input -> IO (Either Aeson.Value output))
lowerThrowingFunToValueFun fun = withFrozenCallStack $ \inputEither ->
  let inner =
        case inputEither of
          Left err -> throw err
          Right input -> Right <$> fun input
  in
  inner
  & checkpointStrict
  & handle (pure . Left . SerializableException.exceptionToValue)

lowerJSON 
  :: (HasCallStack
  , Curry.Callable fun
  , input ~ Curry.Params fun
  , IO output ~ Curry.Output fun
  , Curry.IsTuple input
  , NFData input
  , NFData output
  , Aeson.FromJSON input
  , Aeson.ToJSON output
  )
  => fun -> VirgilFunction
lowerJSON fun =
  lowerJSONUncurried (Curry.uncurryN fun)

lowerCBOR
  :: (HasCallStack
  , Curry.Callable fun
  , input ~ Curry.Params fun
  , IO output ~ Curry.Output fun
  , Curry.IsTuple input
  , NFData input
  , NFData output
  , Aeson.FromJSON input
  , Aeson.ToJSON output
  )
  => fun -> VirgilFunction
lowerCBOR fun =
  lowerCBORUncurried (Curry.uncurryN fun)

liftJSON
  :: (HasCallStack, Curry.Callable fun, input ~ Curry.Params fun, IO output ~ Curry.Output fun, Curry.IsTuple input, Aeson.ToJSON input, Aeson.FromJSON output, NFData output
  )
  => VirgilFunction
  -> fun
liftJSON = Curry.curryN . liftJSONUncurried

liftCBOR
  :: (HasCallStack, Curry.Callable fun, input ~ Curry.Params fun, IO output ~ Curry.Output fun, Curry.IsTuple input, Aeson.ToJSON input, Aeson.FromJSON output, NFData output
  )
  => VirgilFunction
  -> fun
liftCBOR = Curry.curryN . liftCBORUncurried

liftJSONUncurried :: (HasCallStack, Curry.IsTuple input, Aeson.ToJSON input, Aeson.FromJSON output, NFData output) => VirgilFunction -> (input -> IO output)
liftJSONUncurried virgilFun = withFrozenCallStack $
    virgilFun
    & liftVirgilFunToBytestringFun
    & liftBytestringFunToJSONFun
    & liftValueFunToThrowingFun

liftCBORUncurried :: (HasCallStack, Curry.IsTuple input, Aeson.ToJSON input, Aeson.FromJSON output, NFData output) => VirgilFunction -> (input -> IO output)
liftCBORUncurried virgilFun = withFrozenCallStack $
    virgilFun
    & liftVirgilFunToBytestringFun
    & liftBytestringFunToCBORFun
    & liftValueFunToThrowingFun

lowerJSONUncurried :: (HasCallStack, Curry.IsTuple input, NFData input, NFData output, Aeson.FromJSON input, Aeson.ToJSON output) => (input -> IO output) -> VirgilFunction
lowerJSONUncurried throwingFun = withFrozenCallStack $
    throwingFun
    & lowerThrowingFunToValueFun
    & lowerJSONFunToBytestringFun
    & lowerBytestringFunToVirgilFun

lowerCBORUncurried ::(HasCallStack, Curry.IsTuple input, NFData input, NFData output, Aeson.FromJSON input, Aeson.ToJSON output) => (input -> IO output) -> VirgilFunction
lowerCBORUncurried throwingFun = withFrozenCallStack $
    throwingFun
    & lowerThrowingFunToValueFun
    & lowerCBORFunToBytestringFun
    & lowerBytestringFunToVirgilFun

-- | Make sure any latent exceptions are thrown at this point, and get a call stack attached if that was not yet the case
checkpointStrict :: (HasCallStack, NFData a, MonadUnliftIO m) => m a -> m a
checkpointStrict inner = (inner >>= evaluateDeep) & checkpointCallStack

