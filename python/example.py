import virgil
import os
from typing import Callable, Any

dll = virgil.DynamicLibrary(f"{os.path.dirname(__file__)}/libExample.so")

# Examples:

haskellExclaim = dll.liftCBOR("exclaim")
def exclaim(string: str) -> str:
  """
  Add some exclamation marks to a string (secretly using Haskell)
  """
  return haskellExclaim(string)

haskellSumIntegers = dll.liftCBOR("sumIntegers")
def sumIntegers(ints: list[int]) -> int:
  """
  Sum a list of integers (secretly using Haskell) 
  """
  return haskellSumIntegers(ints)

haskellDivIntegers = dll.liftCBOR("divIntegers")
def divIntegers(left: int, right: int) -> int:
  """
  Divide two integers (secretly using Haskell)
  """
  return haskellDivIntegers(left, right)

haskellMappy = dll.liftCBOR("mappy")
def mappy(elems: list[Any], fun: Callable[[Any], Any]) -> list[Any]:
  """
  Call a Python function on each element of an integer list,
  returning the resulting list.
  """
  return haskellMappy(elems, dll.lowerCBOR(fun))

