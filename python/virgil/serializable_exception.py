import tblib
from typing import Any

class HaskellException(Exception):
  def __init__(self, name, message, callstack, annotations = []):
    traceback = haskellCallstackToPythonTraceback(callstack)
    if traceback is not None:
      self.__traceback__ = traceback
    for annotation in annotations:
      self.add_note(annotation)
    super().__init__(name, message)

def haskellCallstackToPythonTraceback(callstack):
  # print(callstack)
  if callstack is None:
    return None
  tb_next = None
  for name, info in callstack:
    code = {'co_filename': info['file'], 'co_name': name}
    frame = {'f_lineno': info['line'], 'f_code': code, 'f_globals': {}}
    current = {'tb_frame': frame, 'tb_lineno': info['line'], 'tb_next': tb_next}
    tb_next = current
  if tb_next is not None:
    return tblib.Traceback.from_dict(tb_next).as_traceback()
  else:
    return None

def pythonTracebackToHaskellCallstack(traceback_object):
    import traceback
    stack = traceback.extract_tb(traceback_object)
    return [[frame.name, {'file': frame.filename, 'line': frame.lineno, 'col': 0}] for frame in stack]

def reraiseHaskellException(name, message, callstack, annotations = []):
  res = None
  # Async exceptions
  if name == "UserInterrupt":
    res = KeyboardInterrupt
  if name == "StackOverflow":
    res = RecursionError(message)
  if name == "HeapOverflow":
    res = MemoryError(message)
  if name == "ThreadKilled": # NOTE: This is not 1:1 the same, but the closest
    res = SystemExit(message)
  # Sync exceptions
  if name == "ArithException" and message == "divide by zero":
    res = ZeroDivisionError
  if name == "ArithException":
    res = ArithmeticError(message)
  # Haskell could not parse the serialized data Python passed to it:
  if name == "InputParseException" or name == "OutputParseException":
    res = ValueError(message)
  
  if res is None:
    raise HaskellException(name, message, callstack, annotations)
  else:
    context = HaskellException(name, message, callstack, annotations)
    raise res from context

def exceptionToValue(exception_info) -> Any:
    exception_name = exception_info[0]
    exception_message = exception_info[1]
    exception_traceback = exception_info[2]
    """
    Turn a Python exception into a value that can be serialized.
    """
    return {'name': exception_name.__name__,
    'message': str(exception_message),
    'callstack': pythonTracebackToHaskellCallstack(exception_traceback),
    'annotations': [], # TODO
     }
