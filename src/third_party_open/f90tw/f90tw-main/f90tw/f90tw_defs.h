#define F90TESTWRAPPERS

/* utility macros */
#define TOSTRHLP(x) #x
#define TOSTR(x) TOSTRHLP(x)

/* fix concatenation in traditional cpp macro definition.
// this is not possible for strigify operation and this is why
// cpp is (or another preprocessor is required for the correct
// preprocessing fortran sources when gfortran is in use. */
#ifdef __GFORTRAN__
#define PASTE(a) a
#define CONCAT(a,b) PASTE(a)b
#else
#define PASTE(a,b) a ## b
#define CONCAT(a,b) PASTE(a,b)
#endif

/* concatenation utilities with w as the separator character. */
#define CONCAT2(a,b,w) CONCAT(a,CONCAT(w,b))
#define CONCAT3(a,b,c,w) CONCAT(a,CONCAT(w,CONCAT2(b,c,w)))
#define CONCAT4(a,b,c,d,w) CONCAT(a,CONCAT(w,CONCAT3(b,c,d,w)))
#define CONCAT5(a,b,c,d,e,w) CONCAT(a,CONCAT(w,CONCAT4(b,c,d,e,w)))
#define CONCAT6(a,b,c,d,e,f,w) CONCAT(a,CONCAT(w,CONCAT5(b,c,d,e,f,w)))
#define PREPENDF90(x) CONCAT2(F90,x,_)
#define REVERSE2(a, b) b,a
#define REVERSE1(a) a
#define GET_MACRO(_0,_1,NAME,...) NAME
#define REVERSE(...) GET_MACRO(__VA_ARGS__,REVERSE2,REVERSE1)(__VA_ARGS__)

#define TESTFRK gtest

/* code generation macros,
// H* : c definition
// C* : c/c++ implementation
// F* : fortran implemenation. */
#define HCODE_( TESTTYPE, TESTSUITENAME, TESTNAME, SUBNAME, ... ) extern "C" {  void SUBNAME(); }
#define HCODE( TESTTYPE, TESTSUITENAME, TESTNAME, SUBNAME, ... ) HCODE_( TESTTYPE,TESTSUITENAME, TESTNAME, SUBNAME, __VA_ARGS__ )
#define CCODEG_( TESTTYPE, TESTSUITENAME, TESTNAME, SUBNAME, ... ) TESTTYPE(TESTSUITENAME,TESTNAME) { SUBNAME(); }
#define CCODE( TESTTYPE, TESTSUITENAME, TESTNAME, SUBNAME, ... ) CCODEG_( TESTTYPE, TESTSUITENAME, TESTNAME, SUBNAME, __VA_ARGS__ )
