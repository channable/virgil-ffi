import ctypes

def BYTE_BOX(dynamic_library):
  class ByteBox(ctypes.Structure):
    _fields_ = [('elems', ctypes.POINTER(ctypes.c_char)), ("size", ctypes.c_uint64)]
    def __init__(self, string = None):
      if string != None:
        self.fill_with(string)

    def fill_with(self, string):
      if isinstance(string, bytes) or isinstance(string, ByteBox):
        bytestring = bytes(string)
      elif isinstance(string, str):
        bytestring = string.encode()
      else:
        raise Exception("Cannot convert input object {string} to str or bytes")
      dynamic_library.alloc.free(self.elems)
      self.elems = dynamic_library.alloc.allocStringCopy(bytestring)
      self.size = len(bytestring)

    def __len__(self):
      return self.size

    def __getitem__(self, key):
      if self.elems is None:
        raise KeyError(key)
      return self.elems[0:self.size].__getitem__(key)

    def __bytes__(self):
      if self.elems is None:
        return b''
      return self.elems[0:self.size]

    def __str__(self):
      return bytes(self).decode()
  return ByteBox
