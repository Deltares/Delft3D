#ifdef F90TESTWRAPPERS

/* For each supported gtest assertion, a wrapper is created for
// the various levels (see gtest_level.h) and arguments type (see
// <implement|interfae>_gtest_wrapers.h) supported. The latter is
// controlled with HAVECINT, HAVECBOOL, HAVECFLOAT, HAVECDOUBLE and
// DOSTR definitions corresponding to integer(kind=C_INT),
// logical(kind=C_BOOL), real(kind=C_FLOAT), real(kind=C_DOUBLE) and
// character(LEN=C_CHAR) types. The GTESTFUNCTION2M and GTESTFUNCTION2
// marcos control the specific implementation of the wrapper (with and
// without a custom message). The name of the arguments is controlled
// by NAME1 and NAME2 macros. */

/* set variable names for the arguments */
#define NAME1 val1
#define NAME2 val2
/* set the definitions of the gtest functions of the wrapper */
#define GTESTFUNCTION GTESTFUNCTION2
#define GTESTFUNCTIONM GTESTFUNCTION2M
/* set the flags for the arguments type supported for the wrapper*/
#define HAVECBOOL
#define HAVECINT
#define HAVECFLOAT
#define HAVECDOUBLE
#undef HAVECAST
#undef DOSTR

/* set the test arguments. initialize the two arguments, test pass case. */
#define CINTARGSS 1, 1
#define CINTARGSD 1, 2
#define CFLOATARGSS 1.1, 1.1
#define CFLOATARGSD 1.1, 2.1
#define CDOUBLEARGSS 1.1D0, 1.1D0
#define CDOUBLEARGSD 1.1D0, 2.1D0
#define CBOOLARGSS logical(.True.,KIND=C_BOOL), logical(.True.,KIND=C_BOOL)
#define CBOOLARGSD logical(.True.,KIND=C_BOOL), logical(.False.,KIND=C_BOOL)
#define LOGICALARGSS .True., .True.
#define LOGICALARGSD .True., .False.
#define DOTESTS

/* each block creates the implementation of the wrapper.
// The name of the macro is set with WHAT definition, the
// flags for supported arguments type, the appropriate
// implementation macro. */

#define CINTARGS CINTARGSS
#define CFLOATARGS CFLOATARGSS
#define CDOUBLEARGS CDOUBLEARGSS
#define CBOOLARGS CBOOLARGSS
#define LOGICALARGS LOGICALARGSS

#define HAVECAST
/* (LEVEL)_EQ and similars section */
#define WHAT EQ
#include HEADERLEVEL
#undef WHAT

/* (LEVEL)_NE and similars section */
/* test defs */
#undef CINTARGS
#undef CFLOATARGS
#undef CDOUBLEARGS
#undef CBOOLARGS
#undef LOGICALARGS
#define CINTARGS CINTARGSD
#define CFLOATARGS CFLOATARGSD
#define CDOUBLEARGS CDOUBLEARGSD
#define CBOOLARGS CBOOLARGSD
#define LOGICALARGS LOGICALARGSD
/* end test defs */
#define WHAT NE
#include HEADERLEVEL
#undef WHAT
#undef HAVECAST

#undef HAVECBOOL
/* (LEVEL)_LT and similars section */
/* test defs */
#undef CBOOLARGS
#undef LOGICALARGS
/* end test defs */
#define WHAT LT
#include HEADERLEVEL
#undef WHAT

/* (LEVEL)_LE and similars section */
#define WHAT LE
#include HEADERLEVEL
#undef WHAT

/* (LEVEL)_GT and similars section */
/* test defs */
#undef CINTARGS
#undef CFLOATARGS
#undef CDOUBLEARGS
#define CINTARGS REVERSE( CINTARGSD )
#define CFLOATARGS REVERSE( CFLOATARGSD )
#define CDOUBLEARGS REVERSE( CDOUBLEARGSD )
/* end test defs */
#define WHAT GT
#include HEADERLEVEL
#undef WHAT

/* (LEVEL)_GE and similars section */
#define WHAT GE
#include HEADERLEVEL
#undef WHAT

#undef HAVECINT
#undef HAVECDOUBLE
#define HAVEDIM
/* (LEVEL)_FLOAT_EQ and similars section */
/* test defs */
#undef CINTARGS
#undef CFLOATARGS
#undef CDOUBLEARGS
#define CFLOATARGS 0.0, 0.E-8
#define CDOUBLEARGS 0.D0, 0.D-16
#define CFLOAT1DARGS [ 0.0, 0.0 ], [ 0.E-8, 0.E-8 ]
#define CDOUBLE1DARGS [ 0.D0, 0.D0 ], [ 0.D-16, 0.D-16 ]
#define CFLOAT2DARGS [ [ 0.0, 0.0 ], [ 0.0, 0.0 ] ], [ [ 0.E-8, 0.E-8 ], [ 0.E-8, 0.E-8 ] ]
#define CDOUBLE2DARGS [ [ 0.D0, 0.D0 ], [ 0.D0, 0.D0 ] ], [ [ 0.D-16, 0.D-16 ], [ 0.D-16, 0.D-16 ] ]
/* end test defs */
#define WHAT FLOAT_EQ
#include HEADERLEVEL
#undef WHAT

#undef HAVECFLOAT
#define HAVECDOUBLE
/* (LEVEL)_DOUBLE_EQ and similars section */
#define WHAT DOUBLE_EQ
#include HEADERLEVEL
#undef WHAT

#define HAVECFLOAT
#undef GTESTFUNCTION
#undef GTESTFUNCTIONM
#define GTESTFUNCTION GTESTFUNCTION3
#define GTESTFUNCTIONM GTESTFUNCTION3M
/* (LEVEL)_NEAR and similars section */
/* test defs */
#undef CFLOATARGS
#undef CDOUBLEARGS
#undef CFLOAT1DARGS
#undef CDOUBLE1DARGS
#undef CFLOAT2DARGS
#undef CDOUBLE2DARGS
#define CFLOATARGS 0.0, 0.E-8, 0.E7
#define CDOUBLEARGS 0.D0, 0.D-16, 0.D-15
#define CFLOAT1DARGS [ 0.0, 0.0 ], [ 0.E-8, 0.E-8 ], 0.E7
#define CDOUBLE1DARGS [ 0.D0, 0.D0 ], [ 0.D-16, 0.D-16 ], 0.D-15
#define CFLOAT2DARGS [ [ 0.0, 0.0 ], [ 0.0, 0.0 ] ], [ [ 0.E-8, 0.E-8 ], [ 0.E-8, 0.E-8 ] ], 0.E7
#define CDOUBLE2DARGS [ [ 0.D0, 0.D0 ], [ 0.D0, 0.D0 ] ], [ [ 0.D-16, 0.D-16 ], [ 0.D-16, 0.D-16 ] ], 0.D-15
/* end test defs */
#define WHAT NEAR
#include HEADERLEVEL
#undef WHAT
#undef HAVEDIM

#undef HAVECFLOAT
#undef HAVECDOUBLE
#define DOSTR
#undef NAME1
#undef NAME2
#define NAME1 str1
#define NAME2 str2
#undef GTESTFUNCTION
#undef GTESTFUNCTIONM
#define GTESTFUNCTION GTESTFUNCTION2STR
#define GTESTFUNCTIONM GTESTFUNCTION2STRM
/* (LEVEL)_STREQ and similars section (only one dummy TYPE should be defined) */
/* test defs */
#undef CFLOATARGS
#undef CDOUBLEARGS
#undef CFLOAT1DARGS
#undef CDOUBLE1DARGS
#undef CFLOAT2DARGS
#undef CDOUBLE2DARGS
#define CSTRARGS "first" F90CONCAT C_NULL_CHAR, "first" F90CONCAT C_NULL_CHAR
/* end test defs */
#define WHAT STREQ
#include HEADERLEVEL
#undef WHAT

/* (LEVEL)_STRNE and similars section */
/* test defs */
#undef CSTRARGS
#define CSTRARGS "first" F90CONCAT C_NULL_CHAR, "second" F90CONCAT C_NULL_CHAR
/* end test defs */
#define WHAT STRNE
#include HEADERLEVEL
#undef WHAT

/* (LEVEL)_STRCASEEQ and similars section */
/* test defs */
#undef CSTRARGS
#define CSTRARGS "first" F90CONCAT C_NULL_CHAR, "FiRSt" F90CONCAT C_NULL_CHAR
/* end test defs */
#define WHAT STRCASEEQ
#include HEADERLEVEL
#undef WHAT

/* (LEVEL)_STRCASENE and similars section */
/* test defs */
#undef CSTRARGS
#define CSTRARGS "first" F90CONCAT C_NULL_CHAR, "SecOnD" F90CONCAT C_NULL_CHAR
/* end test defs */
#define WHAT STRCASENE
#include HEADERLEVEL
#undef WHAT

#undef CSTRARGS

#undef DOSTR
#undef HAVECFLOAT
#undef HAVECDOUBLE
#define HAVECBOOL
#define HAVECINT
#undef GTESTFUNCTION
#undef GTESTFUNCTIONM
#define GTESTFUNCTION GTESTFUNCTION1
#define GTESTFUNCTIONM GTESTFUNCTION1M
#undef NAME1
#define NAME1 condition
#define HAVECAST
#define HAVEDIM
/* (LEVEL)_TRUE and similars section */
/* test defs */
#define CINTARGS 1
#define CINT1DARGS [ 1, 2 ]
#define CINT2DARGS [ CINT1DARGS, CINT1DARGS ]
#define CBOOLARGS logical(.True.,KIND=C_BOOL)
#define CBOOL1DARGS [ CBOOLARGS, CBOOLARGS ]
#define CBOOL2DARGS [ CBOOL1DARGS, CBOOL1DARGS ]
#define LOGICALARGS .true.
#define LOGICAL1DARGS [ .true., .true. ]
#define LOGICAL2DARGS [ LOGICAL1DARGS, LOGICAL1DARGS ]
/* end test defs */
#define WHAT TRUE
#include HEADERLEVEL
#undef WHAT

/* (LEVEL)_FALSE */
/* test defs */
#undef CINTARGS
#undef CINT1DARGS
#undef CBOOLARGS
#undef LOGICALARGS
#undef LOGICAL1DARGS
#define CINTARGS 0
#define CINT1DARGS [ 0, 0 ]
#define CBOOLARGS logical(.false.,KIND=C_BOOL)
#define LOGICALARGS .false.
#define LOGICAL1DARGS [ .false., .false. ]
/* end test defs */
#define WHAT FALSE
#include HEADERLEVEL
#undef WHAT
#undef HAVEDIM
#undef HAVECAST
#undef DOTESTS

#undef CINTARGS
#undef CINT1DARGS
#undef CINT2DARGS
#undef CBOOLARGS
#undef CBOOL1DARGS
#undef CBOOL2DARGS
#undef LOGICALARGS
#undef LOGICAL1DARGS
#undef LOGICAL2DARGS

#undef HAVECBOOL
#undef HAVECINT
#undef HAVECFLOAT
#undef HAVECDOUBLE
#undef GTESTFUNCTION
#undef GTESTFUNCTIONM
#undef NAME2
#undef NAME1

#endif
