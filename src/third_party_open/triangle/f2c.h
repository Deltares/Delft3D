/* TODO: ifort on Linux needs underscore, but detect this automatically, not hardcoded: */
#define FTN_UNDERSCORE

#if ! defined(WIN32)
#define STDCALL
#if defined (FTN_UNDERSCORE)

#   define TRICALL tricall_

#else

#   define TRICALL tricall

#endif
#else
#   if defined(_MSC_VER)   /* Microsoft Visual C++ */
#      if (_MSC_VER < 1300)   /* versions earlier than V7.0: use CVF naming convention */
#         define STDCALL _stdcall
#      else                   /* V8.0: use C-Default naming convention */
#         define STDCALL
#      endif
#   endif
#endif

