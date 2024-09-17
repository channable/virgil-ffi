{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | N-argument currying and uncurrying for functions.
module Virgil.Curry(Callable(..), IsTuple(..), Params, Output, Single(..)) where

import Data.Aeson qualified as Aeson
import Data.Vector qualified as Vector
import Control.DeepSeq (NFData)
import GHC.Generics ( Generic )

-- | Typeclass expressing the relation between:
-- - fully-uncurried functions taking a single N-tuple parameter 
-- - and curried functions taking N parameters to be fully saturated.
--
-- | Fully Uncurried | Fully Curried         |
-- |-----------------+-----------------------|
-- | () -> a         | a                     |
-- | Single a -> b   | a -> b                |
-- | (a,b) -> c      | a -> b -> c           |
-- | (a,b,c) -> d    | a -> b -> c -> d      |
-- | (a,b,c,d) -> e  | a -> b -> c -> d -> e |
-- etc.
class Callable fun where
  -- | Fully uncurry a function.
  uncurryN :: fun -> (Params fun -> Output fun)
  -- | Fully curry a function
  curryN :: (Params fun -> Output fun) -> fun

-- NOTE: Overlapping type class instances!
-- Is this scary? No! It only restricts usage of the typeclass
-- until Haskell can tell whether the output type of a function 
-- is yet another function or not.
-- i.e. `N` has to be known to call `curryN` or `uncurryN`.
instance {-# OVERLAPPABLE #-} (Params a ~ (), Output a ~ a) => Callable a where
  uncurryN fun () = fun
  curryN fun = fun ()

instance {-# OVERLAPS #-} (Callable b, IsTuple (Params b)) => Callable (a -> b) where
  uncurryN fun params =
    let (first, rest) = pop params in uncurryN (fun first) rest
  curryN fun = \first -> curryN (\rest -> fun (push first rest))

-- | Helper type family to extract all parameter types from a curried function type
type family (Params a) where
  Params (a -> b)  = Prefix a (Params b)
  Params b         = ()

-- | Helper type family to extract the output type from a curried function type
type family (Output a) where
  Output (a -> b)  = Output b
  Output b         = b


-- | Representation of a single-element tuple.
--
-- Similar to `Data.Tuple.Solo` (and with the same laziness properties), but with a crucial difference:
-- The serialized representation of `Single a` is `[a]` whereas for `Solo a` it is `a`.
--
-- This means that the serialization of `()`, `Single a`, `(a,b)`, `(a,b,c)` etc. is done uniformly as a JSON/CBOR list,
-- i.e. the serialization is _self-describing_.
-- This means that it can correctly be ser/de'd from/to the proper tuple shape even in a fully dynamic language.
data Single a = Single a
  deriving (Eq, Ord, Show, Generic, NFData)

instance Aeson.FromJSON a => Aeson.FromJSON (Single a) where
  parseJSON = Aeson.withArray "Array" $ \arr ->
    if Vector.length arr == 1 then do
        parsedVal <- Aeson.parseJSON $ Vector.unsafeHead arr
        pure (Single parsedVal)
    else
        fail $ "Expected a single-element array, but got " <> show arr

instance Aeson.ToJSON a => Aeson.ToJSON (Single a) where
    toJSON (Single a) = Aeson.toJSON [a]
    toEncoding (Single a) = Aeson.toEncoding [a]


-- | Any instance of this typeclass can be used as parameters for a value-level VirgilFunction.
--
-- All parameters are always passed together as a single tuple. 
-- This uncurried form allows us to read/write all parameters from/to a _single bytestring buffer_.
--
--
-- Note the lack of an instance for `Data.Tuple.Solo`. See `Single` for details.
class IsTuple tup where
  -- | Type of this tuple prefixed with one `extraElem`
  type (Prefix elem tup)
  -- | Remove one element from (the front) to turn a triple in a pair etc.
  pop :: Prefix elem tup -> (elem, tup)
  -- | Add an extra element to (the front) to turn a pair into a triple etc.
  push :: elem -> tup -> Prefix elem tup

instance IsTuple () where
  type instance Prefix a () = Single a
  pop (Single a) = (a, ())
  push a () = Single a

instance IsTuple (Single b) where
  type instance Prefix a (Single b) = (a,b)
  pop (a,b) = (a, Single b)
  push a (Single b) = (a,b)
instance IsTuple (b,c) where
  type instance Prefix a (b,c) = (a,b,c)
  pop (a,b,c) = (a, (b,c))
  push a (b,c) = (a,b,c)
instance IsTuple (b,c,d) where
  type instance Prefix a (b,c,d) = (a,b,c,d)
  pop (a,b,c,d) = (a, (b,c,d))
  push a (b,c,d) = (a,b,c,d)
instance IsTuple (b,c,d,e) where
  type instance Prefix a (b,c,d,e) = (a,b,c,d,e)
  pop (a,b,c,d,e) = (a, (b,c,d,e))
  push a (b,c,d,e) = (a,b,c,d,e)
instance IsTuple (b,c,d,e,f) where
  type instance Prefix a (b,c,d,e,f) = (a,b,c,d,e,f)
  pop (a,b,c,d,e,f) = (a, (b,c,d,e,f))
  push a (b,c,d,e,f) = (a,b,c,d,e,f)
instance IsTuple (b,c,d,e,f,g) where
  type instance Prefix a (b,c,d,e,f,g) = (a,b,c,d,e,f,g)
  pop (a,b,c,d,e,f,g) = (a, (b,c,d,e,f,g))
  push a (b,c,d,e,f,g) = (a,b,c,d,e,f,g)
instance IsTuple (b,c,d,e,f,g,h) where
  type instance Prefix a (b,c,d,e,f,g,h) = (a,b,c,d,e,f,g,h)
  pop (a,b,c,d,e,f,g,h) = (a, (b,c,d,e,f,g,h))
  push a (b,c,d,e,f,g,h) = (a,b,c,d,e,f,g,h)
instance IsTuple (b,c,d,e,f,g,h,i) where
  type instance Prefix a (b,c,d,e,f,g,h,i) = (a,b,c,d,e,f,g,h,i)
  pop (a,b,c,d,e,f,g,h,i) = (a, (b,c,d,e,f,g,h,i))
  push a (b,c,d,e,f,g,h,i) = (a,b,c,d,e,f,g,h,i)
instance IsTuple (b,c,d,e,f,g,h,i,j) where
  type instance Prefix a (b,c,d,e,f,g,h,i,j) = (a,b,c,d,e,f,g,h,i,j)
  pop (a,b,c,d,e,f,g,h,i,j) = (a, (b,c,d,e,f,g,h,i,j))
  push a (b,c,d,e,f,g,h,i,j) = (a,b,c,d,e,f,g,h,i,j)
instance IsTuple (b,c,d,e,f,g,h,i,j,k) where
  type instance Prefix a (b,c,d,e,f,g,h,i,j,k) = (a,b,c,d,e,f,g,h,i,j,k)
  pop (a,b,c,d,e,f,g,h,i,j,k) = (a, (b,c,d,e,f,g,h,i,j,k))
  push a (b,c,d,e,f,g,h,i,j,k) = (a,b,c,d,e,f,g,h,i,j,k)
instance IsTuple (b,c,d,e,f,g,h,i,j,k,l) where
  type instance Prefix a (b,c,d,e,f,g,h,i,j,k,l) = (a,b,c,d,e,f,g,h,i,j,k,l)
  pop (a,b,c,d,e,f,g,h,i,j,k,l) = (a, (b,c,d,e,f,g,h,i,j,k,l))
  push a (b,c,d,e,f,g,h,i,j,k,l) = (a,b,c,d,e,f,g,h,i,j,k,l)

