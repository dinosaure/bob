#include <caml/bigarray.h>
#include <caml/threads.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>

#include <errno.h>

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
