import ctypes
import sys

import json
from typing import Any, Callable
import cbor2

from virgil.byte_box import BYTE_BOX
import virgil.alloc
import virgil.serializable_exception


__all__ = ["DynamicLibrary"]

PythonFinalizer = ctypes.CFUNCTYPE(None, ctypes.c_void_p)


class DynamicLibrary():
    """
    Class wrapping a dynamic library.

    The dynamic library has to expose `virgilRealloc` (with the same interface as libc's `realloc`).
    Besides this, all other functions are rexpected to be `VirgilFFI` functions.
    use `liftCBOR`/`liftJSON` to wrap these from Python so you can call them,
    and `lowerCBOR`/`lowerJSON` to turn a Python callable into a callback you can pass to the foreign side.
    """
    def __init__(self, library_path):
      self.library_name = library_path
      # Load the dynamic library on importing of this module. This will initialize the Haskell runtime system.
      self.cdll = ctypes.CDLL(library_path)

      self.alloc = virgil.alloc.AllocHelper(self)
      self._setupFinalization()

      self.ByteBox = BYTE_BOX(self)
      self.ForeignClosure = ctypes.CFUNCTYPE(None, ctypes.POINTER(self.ByteBox), ctypes.POINTER(self.ByteBox))

    def _setupFinalization(self):

      self.unfinalizedPtrs = dict()
      @PythonFinalizer
      def pythonFinalizer(x):
        nonlocal self
        print(f"Finalizing python object {x}")
        self.unfinalizedPtrs.pop(x)
        print(self.unfinalizedPtrs)

      # Make sure the created finalizer closure lives as long as the DLL
      self.pythonFinalizer = pythonFinalizer

      # Register the finalizer with the Haskell runtime system
      self.cdll.virgilRegisterPythonFinalizer.argtypes = [PythonFinalizer]
      self.cdll.virgilRegisterPythonFinalizer.restype = None
      self.cdll.virgilRegisterPythonFinalizer(pythonFinalizer)

    def funNameToVirgilFun(self, fun_name):
        fun = getattr(self.cdll, fun_name)
        fun.argtypes = [ctypes.POINTER(self.ByteBox), ctypes.POINTER(self.ByteBox)]
        fun.restype = None
        return fun


    def liftJSON(self, fun_name):
        """
        Lifts a Haskell function name referring to a Virgil FFI function to a Python function

        It is recommended to use this at the outermost scope of a file,
        such that:
        - Errors will happen when the file is imported rather than when the function is used
        - The wrapping is done only once (e.g. cached)
        """
        return self.liftJSONRaw(self.funNameToVirgilFun(fun_name))

    def liftJSONRaw(self, fun_name):
        """Lifts a Virgil FFI function to a Python function"""
        return self.liftThrowingFunToCurriedFun(self.liftValueFunToThrowingFun(self.liftBytestringFunToJSONFun(self.liftVirgilFunToBytestringFun(fun_name))))

    def liftCBOR(self, fun_name):
        return self.liftCBORRaw(self.funNameToVirgilFun(fun_name))

    def liftCBORRaw(self, fun_name):
        """Lifts a Virgil FFI function to a Python function"""
        return self.liftThrowingFunToCurriedFun(self.liftValueFunToThrowingFun(self.liftBytestringFunToCBORFun(self.liftVirgilFunToBytestringFun(fun_name))))

    def lowerJSON(self, fun):
        """Lowers a Python function to a Virgil FFI function, returning a `ForeignClosure` that can be serialized"""
        return self.ForeignClosure(self.lowerJSONRaw(fun))

    def lowerJSONRaw(self, fun):
        """Lowers a Python function to a Virgil FFI function"""
        return self.lowerBytestringFunToVirgilFun(self.lowerJSONFunToBytestringFun(self.lowerThrowingFunToValueFun(self.lowerCurriedFunToThrowingFun(fun))))

    def lowerCBOR(self, fun):
        """Lowers a Python function to a Virgil FFI function, returning a `ForeignClosure` that can be serialized"""
        return self.ForeignClosure(self.lowerCBORRaw(fun))

    def lowerCBORRaw(self, fun):
        """Lowers a Python function to a Virgil FFI function"""
        return self.lowerBytestringFunToVirgilFun(self.lowerCBORFunToBytestringFun(self.lowerThrowingFunToValueFun(self.lowerCurriedFunToThrowingFun(fun))))

    def liftVirgilFunToBytestringFun(self, lower_fun):
        def fun(in_bytes: bytes) -> bytes:
            in_box = self.ByteBox(in_bytes)
            in_box = self.ByteBox(in_bytes)
            out_box = self.ByteBox()
            lower_fun(in_box, out_box)
            out_bytes = bytes(out_box)
            return out_bytes
        return fun

    def lowerBytestringFunToVirgilFun(self, higher_fun):
        def fun(in_box_ptr, out_box_ptr):
            in_bytes = bytes(in_box_ptr.contents)
            out_bytes = higher_fun(in_bytes)
            out_box_ptr.contents.fill_with(out_bytes)
        return fun

    def liftBytestringFunToJSONFun(self, lower_fun: Callable[[bytes], bytes]) -> Callable[[list[Any]], Any]:
        def fun(params):
            in_bytes = json.dumps(params, default=self.customJSONEncoder)
            out_bytes = lower_fun(in_bytes)
            try:
                out_obj = json.loads(out_bytes)
                return {'Right': out_obj}
            except json.JsonDecodeError as ex:
                return {'Left': ex}
        return fun

    def lowerJSONFunToBytestringFun(self, higher_fun: Callable[[list[Any]], Any]) -> Callable[[bytes], bytes]:
        def fun(in_bytes):
            try:
                in_obj = json.loads(in_bytes)
                in_obj = {'Right': in_obj}
            except json.JSONDecodeError as ex:
                exception_info = sys.exc_info()
                in_obj = {'Left': exception_info}
            out_obj = higher_fun(in_obj)
            out_bytes = json.dumps(out_obj)
            return out_bytes
        return fun

    def liftBytestringFunToCBORFun(self, lower_fun: Callable[[bytes], bytes]) -> Callable[[list[Any]], Any]:
        def fun(params):
            in_bytes = cbor2.dumps(params, default=self.customCBOREncoder)
            out_bytes = lower_fun(in_bytes)
            try:
                out_obj = cbor2.loads(out_bytes)
                return {'Right': out_obj}
            except cbor2.CBORDecodeError as ex:
                return {'Left': ex}
        return fun

    def lowerCBORFunToBytestringFun(self, higher_fun: Callable[[list[Any]], Any]) -> Callable[[bytes], bytes]:
        def fun(in_bytes):
            try:
                in_obj = cbor2.loads(in_bytes)
                in_obj = {'Right': in_obj}
            except cbor2.CBORDecodeError as ex:
                exception_info = sys.exc_info()
                in_obj = {'Left': exception_info}
            out_obj = higher_fun(in_obj)
            out_bytes = cbor2.dumps(out_obj)
            return out_bytes
        return fun

    def liftValueFunToThrowingFun(self, lower_fun: Callable[[list[Any]], Any]) -> Callable[[list[Any]], Any]:
      def fun(params):
        outObject = lower_fun(params)
        if 'Left' in outObject: 
            # Parsing failed, rethrow parse error
            raise Exception(outObject['Left'])
        elif 'Right' in outObject:
            outObject = outObject['Right']
            if 'Left' in outObject and 'name' in outObject['Left'] and 'message' in outObject['Left']:
                # Running callback failed, reraise foreign exception
                error = outObject['Left']
                virgil.serializable_exception.reraiseHaskellException(error['name'], error['message'], error['callstack'], error['annotations'])
            elif 'Right' in outObject:
                # Running callback succeeded, return result
                return outObject['Right']
            else:
                raise Exception(f"JSON in unexpected format returned from Haskell FFI call: {outObject}")
      return fun

    def lowerThrowingFunToValueFun(self, higher_fun: Callable[[list[Any]], Any]) -> Callable[[list[Any]], Any]:
      def fun(params_either):
        if 'Left' in params_either:
          raise Exception(params_either['Left'])
        elif 'Right' in params_either:
          try:
            output = higher_fun(params_either['Right'])
            return {'Right': output}
          except Exception as ex:
            exception_info = sys.exc_info()
            output = virgil.serializable_exception.exceptionToValue(exception_info)
            return {'Left': output}
        else:
          raise Exception(f"Value not in expected format; expected dict with 'Left' or 'Right' key but got: {params_either}")
      return fun

    def liftThrowingFunToCurriedFun(self, lower_fun: Callable[[list[Any]], Any]) -> Callable[[Any], Any]:
      def fun(*params):
          return lower_fun(params)
      return fun 

    def lowerCurriedFunToThrowingFun(self, higher_fun: Callable[[Any], Any]) -> Callable[[list[Any]], Any]:
      def fun(params):
        return higher_fun(*params)
      return fun

    def customJSONEncoder(self, obj: Any):
      if isinstance(obj, self.ForeignClosure):
        ptr = ctypes.cast(obj, ctypes.c_void_p).value
        print("ptr:", ptr)
        self.unfinalizedPtrs[ptr] = obj
        return {'foreignClosureAddr': ptr}

    def customCBOREncoder(self, encoder, obj: Any):
      if isinstance(obj, self.ForeignClosure):
        ptr = ctypes.cast(obj, ctypes.c_void_p).value
        print("ptr:", ptr)
        self.unfinalizedPtrs[ptr] = obj
        encoder.encode({'foreignClosureAddr': ptr})

