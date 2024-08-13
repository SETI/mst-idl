
#ifndef __IDL_TYPE
#define __IDL_TYPE

#ifndef __osf__
  typedef char idl_byte;
  typedef short idl_int;
  typedef long idl_long; 
  typedef float idl_float;
#else
  typedef char idl_byte;
  typedef short idl_int;
  typedef int idl_long;
  typedef float idl_float;
#endif

#endif
