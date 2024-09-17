import ctypes

class AllocHelper():
    def __init__(self, dynamic_library):
        self.dynamic_library = dynamic_library

        # Will break at load-time if realloc does not exist
        self.dynamic_library.cdll.virgilRealloc.argtypes = [ctypes.c_void_p, ctypes.c_size_t]
        self.dynamic_library.cdll.virgilRealloc.restype = ctypes.c_void_p

    def realloc(self, ptr, size):
        return ctypes.cast(self.dynamic_library.cdll.virgilRealloc(ptr, size), ctypes.c_void_p)

    def malloc(self, size):
        return self.realloc(None, size)

    def free(self, ptr):
        self.realloc(ptr, 0)
        return None

    def allocStringCopy(self, string: str | bytes):
        """
        Copy a str or bytes object to a newly-allocated C string.
        The C string will be allocated using the malloc as exposed by the Haskell shared library.

        The string is not null-terminated.
        """
        if isinstance(string, bytes):
            bytestring = string
        if isinstance(string, str):
            bytestring = string.encode("utf-8")

        ptr = ctypes.cast(self.malloc(len(bytestring)), ctypes.POINTER(ctypes.c_char))
        ctypes.memmove(ptr, bytestring, len(bytestring))

        return ptr
