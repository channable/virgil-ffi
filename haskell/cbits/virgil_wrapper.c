// We use the the widely-supported constructor/destructor gcc extension
// to initialize/deinitialize the Haskell RTS on shared-library load/unload.

// Helper macro to turn an identifier into a string literal.
#define STR(a) STR_IMPL(a)
#define STR_IMPL(a) #a

// Included by libc:
#include <stdlib.h>
#include <signal.h>

// Included by cabal when compiling:
#include <HsFFI.h>

static void library_init (void) __attribute__ ((constructor));
static void library_init(void)
{
  /* We fill in the library name in argv so it shows up in stacktraces and to make the GHCRTS envvar work. */
  static int argc = 1;
  static char *argv[] = { STR(MODULE) ".so", "+RTS", "-threaded", NULL };
  char **argv_ptr = argv;


  // Start up Haskell
  // and reset the Python signal handler 
  // that is overwritten by the Haskell RTS startup
  void (*oldhandler)(int) = signal(SIGINT, SIG_DFL);
  hs_init(&argc, &argv_ptr);
  signal(SIGINT, oldhandler);
}

static void library_exit (void) __attribute__ ((destructor));
static void library_exit(void)
{
  hs_exit();
}
