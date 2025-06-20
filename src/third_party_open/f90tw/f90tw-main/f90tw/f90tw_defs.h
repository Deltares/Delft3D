/* check:
// https://stackoverflow.com/questions/127318/is-there-any-sed-like-utility-for-cmd-exe
*/

#define F90TESTWRAPPERS

/*  the version (boost style)
//  F90TW_VERSION % 100 is the patch level
//  F90TW_VERSION / 100 % 1000 is the minor version
//  F90TW_VERSION / 100000 is the major version */
#define F90TW_VERSION 001000

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

/* version */
#define F90_F90TW_VER \
     interface f90tw_version ; \
          subroutine c_f90tw_version( major, minor, patch) BIND(C, name="c_f90tw_version") ; \
               import C_INT ; \
               integer(KIND=C_INT), intent(out) :: major, minor, patch ; \
          end subroutine c_f90tw_version ; \
     end interface f90tw_version

#define C_F90TW_VER \
     extern "C" { \
     void c_f90tw_version(int* major, int* minor, int* patch) { \
          *patch = F90TW_VERSION % 100; \
          *minor = F90TW_VERSION / 100 % 1000; \
          *major = F90TW_VERSION / 100000; \
          } \
     }

/* utility macros for simple boost/gtest test implementation */

/* first lines of module TESTNAME for testing module MODNAME using TESTFRK test framework */
#define F90TESTMODULE(TESTNAME,MODNAME)   \
    module TESTNAME; \
        use CONCAT2(assertions,TESTFRK,_) ;  \
        use MODNAME; \
        implicit none

#define F90TESTCONTAINS contains

/* module TESTNAME closure */
#define F90ENDTESTMODULE(TESTNAME) end module TESTNAME

/* code generation macros,
// H* : c definition
// C* : c/c++ implementation
// F* : fortran implemenation. */
#define HCODE_( TESTTYPE, TESTSUITENAME, TESTNAME, SUBNAME, ... ) extern "C" {  void SUBNAME(); }
#define HCODE( TESTTYPE, TESTSUITENAME, TESTNAME, SUBNAME, ... ) HCODE_( TESTTYPE,TESTSUITENAME, TESTNAME, SUBNAME, __VA_ARGS__ )
#define CCODE_( TESTTYPE, TESTSUITENAME, TESTNAME, SUBNAME, ... ) TESTTYPE(TESTNAME) { SUBNAME(); }
#define CCODEG_( TESTTYPE, TESTSUITENAME, TESTNAME, SUBNAME, ... ) TESTTYPE(TESTSUITENAME,TESTNAME) { SUBNAME(); }
#define CCODEFULL_( TESTTYPE, TESTSUITENAME, TESTNAME, SUBNAME, ... ) TESTTYPE(TESTNAME) { do { __VA_ARGS__ } while(false); }
#define CCODE( TESTTYPE, TESTSUITENAME, TESTNAME, SUBNAME, ... ) CCODEG_( TESTTYPE, TESTSUITENAME, TESTNAME, SUBNAME, __VA_ARGS__ )
#define FCODE_( TESTTYPE, TESTSUITENAME, TESTNAME, SUBNAME, ... ) subroutine SUBNAME() BIND(C,name=#SUBNAME); __VA_ARGS__ ; end subroutine SUBNAME
#define FCODE( TESTTYPE, TESTSUITENAME, TESTNAME, SUBNAME, ... ) FCODE_( TESTTYPE, TESTSUITENAME, TESTNAME, SUBNAME, __VA_ARGS__ )
