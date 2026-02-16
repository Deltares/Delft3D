
#ifndef PRECICE_EXPORT
#define PRECICE_EXPORT

#ifdef PRECICE_STATIC_DEFINE
#  define PRECICE_API
#  define PRECICE_NO_EXPORT
#else
#  ifndef PRECICE_API
#    ifdef preciceCore_EXPORTS
        /* We are building this library */
#      define PRECICE_API __declspec(dllexport)
#    else
        /* We are using this library */
#      define PRECICE_API __declspec(dllimport)
#    endif
#  endif

#  ifndef PRECICE_NO_EXPORT
#    define PRECICE_NO_EXPORT 
#  endif
#endif

#ifndef PRECICE_DEPRECATED
#  define PRECICE_DEPRECATED __declspec(deprecated)
#endif

#ifndef PRECICE_DEPRECATED_EXPORT
#  define PRECICE_DEPRECATED_EXPORT PRECICE_API PRECICE_DEPRECATED
#endif

#ifndef PRECICE_DEPRECATED_NO_EXPORT
#  define PRECICE_DEPRECATED_NO_EXPORT PRECICE_NO_EXPORT PRECICE_DEPRECATED
#endif

/* NOLINTNEXTLINE(readability-avoid-unconditional-preprocessor-if) */
#if 0 /* DEFINE_NO_DEPRECATED */
#  ifndef PRECICE_NO_DEPRECATED
#    define PRECICE_NO_DEPRECATED
#  endif
#endif

#endif /* PRECICE_EXPORT */
