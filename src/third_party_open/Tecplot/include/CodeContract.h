/*
******************************************************************
******************************************************************
*******                                                   ********
******  (C) 1988-2008 Tecplot, Inc.                        *******
*******                                                   ********
******************************************************************
******************************************************************
*/

#pragma once

#include <cstdlib>
#include <iostream>
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

#ifndef VALID_REF
#ifdef MSWIN
    #define VALID_REF(p)      ((p)  != 0)
    #define VALID_FN_REF(pf)  ((pf) != 0)
#else
    #define VALID_REF(p)      ((p)  != 0)
    #define VALID_FN_REF(pf)  ((pf) != 0)
#endif
#endif

#if defined NO_ASSERTS
    #ifndef ASSERT
        #define ASSERT(EXPR)
    #endif

    /**
     * If a project is compiled with "warnings treated as errors" we need a way to to remove
     * parameters or declarations that are only used for assertions if assertions are turned off.
     *
     * This macro is used in the implementation as follows for parameters:
     *
     *     void someFunction(int const ASSERT_ONLY(paramOnlyUsedInAssertions))
     *     {
     *        ...
     *     }
     *
     * or for as follows for declarations:
     *
     *     ASSERT_ONLY(int const someResult =)
     *         someFunctionCall(arg);
     *     CHECK(someResult == expectedValue);
     */
    #ifndef ASSERT_ONLY
        #define ASSERT_ONLY(EXPR)
    #endif
#else
    #include <assert.h>
    #ifndef ASSERT
        #define ASSERT(EXPR) assert(EXPR)
    #endif

    /*
     * See note above for this macro.
     */

    #ifndef ASSERT_ONLY
        #define ASSERT_ONLY(EXPR) EXPR
    #endif
#endif

/**
 * @deprecated use ASSERT_ONLY instead
 */
#if !defined ASSERT_ONLY_PARAM
#define ASSERT_ONLY_PARAM(PARAMETER) ASSERT_ONLY(PARAMETER)
#endif

#if !defined INVARIANT
    #define INVARIANT(EXPR) ASSERT(EXPR)
#endif
#if !defined REQUIRE
    #define REQUIRE(EXPR)   ASSERT(EXPR)
#endif
#if !defined ENSURE
    #define ENSURE(EXPR)    ASSERT(EXPR)
#endif
#if !defined CHECK
    #define CHECK(EXPR)     ASSERT(EXPR)
#endif

#ifdef VERIFY
    #undef VERIFY
#endif
#ifndef VERIFY
    #if defined NO_ASSERTS
        #define VERIFY(EXPR) ((void)(EXPR))
    #elif defined NDEBUG
        #define VERIFY(EXPR) \
            do \
            { \
                bool _result = (EXPR); \
                if (!_result) \
                { \
                    std::cerr << __FILE__ << ':' << __LINE__ << ':' << "Assertion '" << #EXPR << "' failed."; \
                    abort(); \
                } \
            } while(0);
    #else
        #define VERIFY(EXPR) assert(EXPR)
    #endif
#endif /* VERIFY */

/* convenience macros for implication, P -> Q, and equivalence, P <-> Q. */
#if !defined IMPLICATION
    #define IMPLICATION(P,Q) (!(P) || (Q))
#endif
#if !defined EQUIVALENCE
    #define EQUIVALENCE(P,Q) ((P) == (Q))
#endif

#define VALID_MAP_KEY(key,map) (map.find(key) != map.end())

/* Used for legacy code only */
#if !defined VALID_REF_OR_NULL
    #define VALID_REF_OR_NULL(p)       (VALID_REF(p) || p == 0)
#endif
#if !defined VALID_BOOLEAN
    #define VALID_BOOLEAN(b)           ((b) == 1 || (b) == 0)
#endif
#if !defined VALID_ENUM
    #define VALID_ENUM(value, type)    (0 <= (value) && (value) < END_##type)
#endif
#if !defined VALID_CLASS_ENUM
    #define VALID_CLASS_ENUM(value, class, type)    (0 <= (value) && (value) < class::END_##type)
#endif

