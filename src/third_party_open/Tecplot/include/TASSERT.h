#ifndef TASSERT_H
#define TASSERT_H
/*
******************************************************************
******************************************************************
*******                                                   ********
******  (C) 1988-2010 Tecplot, Inc.                        *******
*******                                                   ********
******************************************************************
******************************************************************
*/
/*
 * Provide four levels of assertion control. Assertions provide a mechanism
 * to enforce a contract between a client and service provider. The assertions
 * are listed in order of highest to lowest priority. Assertions can be turned
 * off individually by defining the appropriate name (see preprossessor
 * definitions below), however, lower priority assertions should be turned
 * off prior to higher ones. As confidence in the code increases all assertions
 * can be turned off by defining NO_ASSERTS.
 *
 * The assertions defined below have the following meanings:
 *
 *     INVARIANT - Asserts that a property's state is invariant throughout the
 *                 life of the property's scope. Stating invariant properties
 *                 of an application provides a deeper understanding of the
 *                 application's state.  These statements are usually
 *                 positioned just ahead of the preconditions and just after
 *                 the postconditions.
 *
 *     REQUIRE   - Asserts that a method's preconditions are within their
 *                 valid domains. Preconditions are conditions placed upon
 *                 any state information relied upon for the call. These
 *                 statements should be as close to the top of the method
 *                 as possible (except for assertions on invariant properties).
 *
 *     ENSURE    - Asserts that a method's postconditions are within their
 *                 valid ranges. Postconditions are conditions placed upon
 *                 any state information modified by the call. These
 *                 statements should be as close to the bottom of the method
 *                 (presumably there is only one exit point) as possible
 *                 (except for assertions on invariant properties).
 *
 *     CHECK     - Any other assertion not covered by the above assertions.
 *                 These are often added within a method body to specify
 *                 something that may not be immediately obvious to the reader
 *                 or to validate your assumptions about a call to a 3rd party
 *                 method that does not use runtime assertions for its
 *                 preconditions or postconditions. Obviously if the 3rd party
 *                 method uses assertions then there is no need for the CHECK.
 *
 * Additionally a convenience macro is available to place in code that is
 * pending implementation.
 *
 *     NOT_IMPLEMENTED - Assertion that always fails during runtime for debug
 *                       builds and always fails at compile time for release
 *                       builds.
 */


#if !defined TECPLOTKERNEL && !defined STD_ASSERTS
#define STD_ASSERTS
#endif

#  include <assert.h>
#  if !defined ASSERT
#    define ASSERT assert
#  endif

#if defined MSWIN
/* MFC .NET defines ENSURE, so we undefine it here */
#if defined ENSURE
#undef ENSURE
#endif /* ENSURE */
#endif /* MSWIN */

/* CORE SOURCE CODE REMOVED */

#ifdef UNIXX
/* CORE SOURCE CODE REMOVED */

/* CORE SOURCE CODE REMOVED */
#  if !defined VALID_REF
#    define VALID_REF(p)      ( (p)  != NULL )
#  endif
#  if !defined VALID_FN_REF
#    define VALID_FN_REF(fp)  ( (fp) != NULL )
#  endif
/* CORE SOURCE CODE REMOVED */

/* CORE SOURCE CODE REMOVED */
#endif /* UNIXX */

#ifdef MSWIN
/* CORE SOURCE CODE REMOVED */

#if defined NO_ASSERTS
/* release build in TecUtil layer uses these for TUASSERT */
#  if !defined VALID_REF
#    define VALID_REF(p)      ((p)  != NULL)
#  endif
#  if !defined VALID_FN_REF
#    define VALID_FN_REF(pf)  ((pf) != NULL)
#  endif
#else
#  if !defined VALID_REF
#    define VALID_REF(p)      ((p)  != NULL && !IsBadReadPtr((const void *)(p), 1))
#  endif
#  if !defined VALID_FN_REF
#    define VALID_FN_REF(pf)  ((pf) != NULL && !IsBadReadPtr((const void *)(pf),(UINT_PTR)sizeof(const void*)))
#  endif
#endif

/* CORE SOURCE CODE REMOVED */
#endif /* MSWIN */
/* CORE SOURCE CODE REMOVED */
/* other useful validity checks */
#if !defined VALID_BOOLEAN
#  define VALID_BOOLEAN(b)           ((b) == TRUE || (b) == FALSE)
#endif
#if !defined VALID_ENUM
#  define VALID_ENUM(value, type)    (0 <= (int)(value) && \
                                           (int)(value) < END_##type)
#endif

/* Test a parameter than can be NULL or a valid pointer */
#if !defined VALID_REF_OR_NULL
#  define VALID_REF_OR_NULL(ptr) IMPLICATION((ptr) != NULL, VALID_REF(ptr))
#endif
#if !defined VALID_FN_REF_OR_NULL
#  define VALID_FN_REF_OR_NULL(ptr) IMPLICATION((ptr) != NULL, VALID_FN_REF(ptr))
#endif

/* CORE SOURCE CODE REMOVED */

/* Check for a non-zero length string */
#if !defined VALID_NON_ZERO_LEN_STR
#  if defined MSWIN
#    if defined NO_ASSERTS
#      define VALID_NON_ZERO_LEN_STR(str) (VALID_REF(str) && !ISEMPTYSTRING(str))
#    else
#      define VALID_NON_ZERO_LEN_STR(str) \
           (VALID_REF(str)                                                            && \
           !IsBadReadPtr((const void*)(str),(UINT_PTR)(1+strlen((const char*)(str)))) && \
           !ISEMPTYSTRING(str))
#    endif
#  else
#    define VALID_NON_ZERO_LEN_STR(str) (VALID_REF(str) && !ISEMPTYSTRING(str))
#  endif
#endif

#if !defined VALID_SET_INDEX
#  define VALID_SET_INDEX(setIndex) (((SetIndex_t)setIndex)>=(SetIndex_t)1)
#endif

/* Check for valid stdio file handle */
#if !defined VALID_FILE_HANDLE
#  define VALID_FILE_HANDLE(stream) ((stream) != NULL)
#endif

/* To check colors and pen numbers */
/* CORE SOURCE CODE REMOVED */

#if defined NO_ASSERTS
/* CORE SOURCE CODE REMOVED */
#   if !defined INVARIANT
#     define INVARIANT(EXPR)
#   endif
#   if !defined REQUIRE
#     define REQUIRE(EXPR)
#   endif
#   if !defined ENSURE
#     define ENSURE(EXPR)
#   endif
#   if !defined CHECK
#     define CHECK(EXPR)
#   endif
#   ifdef VERIFY
#     undef VERIFY
#   endif
#   define VERIFY(EXPR)    ((void)(EXPR))

/*
 * If a project is compiled with "warnings treated as errors" we need a way to
 * to remove parameters that are only used for assertions if assertions are
 * turned off.
 *
 * This macro is used in the implementation as follows:
 *
 *     void someFunction(int const ASSERT_ONLY(paramOnlyUsedInAssertions))
 *     {
 *        ...
 *     }
 */
#   if !defined ASSERT_ONLY
#     define ASSERT_ONLY(PARAMETER)
#   endif

/*
 * Only define IGNORENOTIMPLEMENTED if building a "test" release build
 * that you are fully aware may contain unimplemented features.
 */
#   if !defined NOT_IMPLEMENTED
#     if defined IGNORENOTIMPLEMENTED
#       define NOT_IMPLEMENTED() CHECK(FALSE)
#     else
#       if defined MSWIN
/*
 * NOT_IMPLEMENTED is defined using a parameter, but should be called with none,
 * this will then throw a warning and not break the compile. Unix doesn't pick
 * up this warning, so break the compile under Unix
 */
#         define NOT_IMPLEMENTED(x)  TAssert("Not Implemented", __FILE__, __LINE__)
#       endif
#       if defined UNIXX
#         define NOT_IMPLEMENTED()  not implemented /* intentionally break the compile */
#       endif
#     endif
#   endif
#elif defined STD_ASSERTS
/* CORE SOURCE CODE REMOVED */
#   if !defined INVARIANT
#     define INVARIANT(EXPR)       assert(EXPR)
#   endif
#   if !defined REQUIRE
#     define REQUIRE(EXPR)         assert(EXPR)
#   endif
#   if !defined ENSURE
#     define ENSURE(EXPR)          assert(EXPR)
#   endif
#   if !defined CHECK
#     define CHECK(EXPR)           assert(EXPR)
#   endif
#   ifdef VERIFY
#     undef VERIFY
#   endif
#   ifndef VERIFY
#     if defined NDEBUG
#       define VERIFY(EXPR) ((void)(EXPR))
#     else
#       define VERIFY(EXPR) assert(EXPR)
#     endif
#   endif /* VERIFY */
#   if !defined NOT_IMPLEMENTED
#     define NOT_IMPLEMENTED()     assert(!("Not Implemented"))
#   endif
    /*
     * See note above for this macro.
     */
#   if !defined ASSERT_ONLY
#     define ASSERT_ONLY(PARAMETER) PARAMETER
#   endif
#else
/* CORE SOURCE CODE REMOVED */
#endif

/**
 * @deprecated use ASSERT_ONLY instead
 */
#if !defined ASSERT_ONLY_PARAM && defined ASSERT_ONLY
#   define ASSERT_ONLY_PARAM(PARAMETER) ASSERT_ONLY(PARAMETER)
#endif

/* CORE SOURCE CODE REMOVED */

/* convenience macros for implication, P -> Q, and equivalence, P <-> Q. */
#if !defined IMPLICATION
#  define IMPLICATION(P,Q) (!(P) || (Q))
#endif
#if !defined EQUIVALENCE
#  define EQUIVALENCE(P,Q) ((P) == (Q))
#endif

/* CORE SOURCE CODE REMOVED */


#endif /* TASSERT_H */
