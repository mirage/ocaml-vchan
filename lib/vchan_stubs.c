#include <stdint.h>
#include <caml/mlvalues.h>

CAMLprim value stub_atomic_or_fetch(value ptr, value val)
{
  uint8_t c_ptr = Int_val(ptr), c_val = Int_val(val);

  return Int_val(__atomic_or_fetch(&c_ptr, c_val, __ATOMIC_SEQ_CST));
}

CAMLprim value stub_atomic_fetch_and(value ptr, value val)
{
  uint8_t c_ptr = Int_val(ptr), c_val = Int_val(val);

  return Int_val(__atomic_fetch_and(&c_ptr, c_val, __ATOMIC_SEQ_CST));
}
