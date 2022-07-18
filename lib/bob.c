#include <caml/bigarray.h>
// #include <caml/threads.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>

#include <errno.h>
#include <string.h>

#ifndef __unused
# if defined(_MSC_VER) && _MSC_VER >= 1500
#  define __unused(x) __pragma( warning (push) ) \
    __pragma( warning (disable:4189 ) ) \
    x \
    __pragma( warning (pop))
# else
#  define __unused(x) x __attribute__((unused))
# endif
#endif
#define __unit() value __unused(unit)

CAMLprim value
bob_retrieve_error(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(res);
  res = unix_error_of_code (errno);
  CAMLreturn(res);
}

extern void caml_enter_blocking_section (void);
extern void caml_leave_blocking_section (void);

CAMLprim value
bob_bigstring_read(value fd, value ba, value off, value len) {
  int ret;
  uint8_t *buf = ((uint8_t*) Caml_ba_data_val (ba)) + Long_val (off);
  long max = Long_val (len);

  caml_enter_blocking_section();
  ret = read(Int_val (fd), buf, max);
  caml_leave_blocking_section();

  return Val_int (ret);
}

CAMLprim value
bob_bigstring_write(value fd, value ba, value off, value len) {
  int ret;
  uint8_t *buf = ((uint8_t*) Caml_ba_data_val (ba)) + Long_val (off);
  long max = Long_val (len);

  caml_enter_blocking_section();
  ret = write(Int_val (fd), buf, max);
  caml_leave_blocking_section();

  return Val_int (ret);
}

CAMLprim value
bob_is_windows(__unit ()) {
#if defined(__ESPERANTO__)
#include "cosmopolitan.h"
  return Val_bool (IsWindows());
#else
#include <caml/s.h>
  return Val_bool (strcmp(OCAML_OS_TYPE, "Win32") == 0);
#endif
}

/* XXX(dinosaure): this function is used only on Windows for [connect()].
 * Indeed, despite Linux, we must set a socket as a non-blocking socket and
 * execute [connect()] on it. Then, use [select()] to signal us when the socket
 * is ready. On Linux, it's different when the socket is already writable. In
 * such case, [select()] tells to us that the socket is already ready to
 * [connect()]. Moreover, the semantic of our [Fiber.connect] is not so
 * clear and we should specify it more strictly. TODO! */
CAMLprim value
bob_set_nonblock(value fd, value mode) {
#if defined(__ESPERANTO__)
#include "cosmopolitan.h"
  if (IsWindows()) {
    uint32_t opt = Bool_val (mode);
    __sys_ioctlsocket_nt(Int_val (fd), FIONBIO, &opt);
  }
#endif
  return Val_unit;
}
