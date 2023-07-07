#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* ts.c */
/* Fortran interface file */

/*
* This file was generated automatically by bfort from the C source
* file.  
 */

#ifdef PETSC_USE_POINTER_CONVERSION
#if defined(__cplusplus)
extern "C" { 
#endif 
extern void *PetscToPointer(void*);
extern int PetscFromPointer(void *);
extern void PetscRmPointer(void*);
#if defined(__cplusplus)
} 
#endif 

#else

#define PetscToPointer(a) (*(PetscFortranAddr *)(a))
#define PetscFromPointer(a) (PetscFortranAddr)(a)
#define PetscRmPointer(a)
#endif

#include "petscts.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssetfromoptions_ TSSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssetfromoptions_ tssetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgettrajectory_ TSGETTRAJECTORY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgettrajectory_ tsgettrajectory
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssetsavetrajectory_ TSSETSAVETRAJECTORY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssetsavetrajectory_ tssetsavetrajectory
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tscomputerhsjacobian_ TSCOMPUTERHSJACOBIAN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tscomputerhsjacobian_ tscomputerhsjacobian
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tscomputerhsfunction_ TSCOMPUTERHSFUNCTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tscomputerhsfunction_ tscomputerhsfunction
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tscomputesolutionfunction_ TSCOMPUTESOLUTIONFUNCTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tscomputesolutionfunction_ tscomputesolutionfunction
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tscomputeforcingfunction_ TSCOMPUTEFORCINGFUNCTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tscomputeforcingfunction_ tscomputeforcingfunction
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tscomputeifunction_ TSCOMPUTEIFUNCTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tscomputeifunction_ tscomputeifunction
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tscomputeijacobian_ TSCOMPUTEIJACOBIAN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tscomputeijacobian_ tscomputeijacobian
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsrhsjacobiansetreuse_ TSRHSJACOBIANSETREUSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsrhsjacobiansetreuse_ tsrhsjacobiansetreuse
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tscomputei2function_ TSCOMPUTEI2FUNCTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tscomputei2function_ tscomputei2function
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tscomputei2jacobian_ TSCOMPUTEI2JACOBIAN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tscomputei2jacobian_ tscomputei2jacobian
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ts2setsolution_ TS2SETSOLUTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ts2setsolution_ ts2setsolution
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ts2getsolution_ TS2GETSOLUTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ts2getsolution_ ts2getsolution
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssetapplicationcontext_ TSSETAPPLICATIONCONTEXT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssetapplicationcontext_ tssetapplicationcontext
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetapplicationcontext_ TSGETAPPLICATIONCONTEXT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetapplicationcontext_ tsgetapplicationcontext
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetstepnumber_ TSGETSTEPNUMBER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetstepnumber_ tsgetstepnumber
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssetstepnumber_ TSSETSTEPNUMBER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssetstepnumber_ tssetstepnumber
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssettimestep_ TSSETTIMESTEP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssettimestep_ tssettimestep
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssetexactfinaltime_ TSSETEXACTFINALTIME
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssetexactfinaltime_ tssetexactfinaltime
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetexactfinaltime_ TSGETEXACTFINALTIME
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetexactfinaltime_ tsgetexactfinaltime
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgettimestep_ TSGETTIMESTEP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgettimestep_ tsgettimestep
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetsolution_ TSGETSOLUTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetsolution_ tsgetsolution
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetsolutioncomponents_ TSGETSOLUTIONCOMPONENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetsolutioncomponents_ tsgetsolutioncomponents
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetauxsolution_ TSGETAUXSOLUTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetauxsolution_ tsgetauxsolution
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgettimeerror_ TSGETTIMEERROR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgettimeerror_ tsgettimeerror
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssettimeerror_ TSSETTIMEERROR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssettimeerror_ tssettimeerror
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssetproblemtype_ TSSETPROBLEMTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssetproblemtype_ tssetproblemtype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssetup_ TSSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssetup_ tssetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsreset_ TSRESET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsreset_ tsreset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsdestroy_ TSDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsdestroy_ tsdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetsnes_ TSGETSNES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetsnes_ tsgetsnes
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssetsnes_ TSSETSNES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssetsnes_ tssetsnes
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetksp_ TSGETKSP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetksp_ tsgetksp
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssetmaxsteps_ TSSETMAXSTEPS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssetmaxsteps_ tssetmaxsteps
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetmaxsteps_ TSGETMAXSTEPS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetmaxsteps_ tsgetmaxsteps
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssetmaxtime_ TSSETMAXTIME
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssetmaxtime_ tssetmaxtime
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetmaxtime_ TSGETMAXTIME
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetmaxtime_ tsgetmaxtime
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssetinitialtimestep_ TSSETINITIALTIMESTEP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssetinitialtimestep_ tssetinitialtimestep
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetduration_ TSGETDURATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetduration_ tsgetduration
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssetduration_ TSSETDURATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssetduration_ tssetduration
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgettimestepnumber_ TSGETTIMESTEPNUMBER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgettimestepnumber_ tsgettimestepnumber
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgettotalsteps_ TSGETTOTALSTEPS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgettotalsteps_ tsgettotalsteps
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssetsolution_ TSSETSOLUTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssetsolution_ tssetsolution
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsprestep_ TSPRESTEP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsprestep_ tsprestep
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsprestage_ TSPRESTAGE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsprestage_ tsprestage
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tspoststage_ TSPOSTSTAGE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tspoststage_ tspoststage
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tspostevaluate_ TSPOSTEVALUATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tspostevaluate_ tspostevaluate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tspoststep_ TSPOSTSTEP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tspoststep_ tspoststep
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsinterpolate_ TSINTERPOLATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsinterpolate_ tsinterpolate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsstep_ TSSTEP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsstep_ tsstep
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsevaluatewlte_ TSEVALUATEWLTE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsevaluatewlte_ tsevaluatewlte
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsevaluatestep_ TSEVALUATESTEP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsevaluatestep_ tsevaluatestep
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssolve_ TSSOLVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssolve_ tssolve
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgettime_ TSGETTIME
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgettime_ tsgettime
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetprevtime_ TSGETPREVTIME
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetprevtime_ tsgetprevtime
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssettime_ TSSETTIME
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssettime_ tssettime
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssetdm_ TSSETDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssetdm_ tssetdm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetdm_ TSGETDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetdm_ tsgetdm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snestsformfunction_ SNESTSFORMFUNCTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snestsformfunction_ snestsformfunction
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snestsformjacobian_ SNESTSFORMJACOBIAN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snestsformjacobian_ snestsformjacobian
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetequationtype_ TSGETEQUATIONTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetequationtype_ tsgetequationtype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssetequationtype_ TSSETEQUATIONTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssetequationtype_ tssetequationtype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetconvergedreason_ TSGETCONVERGEDREASON
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetconvergedreason_ tsgetconvergedreason
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssetconvergedreason_ TSSETCONVERGEDREASON
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssetconvergedreason_ tssetconvergedreason
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetsolvetime_ TSGETSOLVETIME
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetsolvetime_ tsgetsolvetime
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetsnesiterations_ TSGETSNESITERATIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetsnesiterations_ tsgetsnesiterations
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetkspiterations_ TSGETKSPITERATIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetkspiterations_ tsgetkspiterations
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetsteprejections_ TSGETSTEPREJECTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetsteprejections_ tsgetsteprejections
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetsnesfailures_ TSGETSNESFAILURES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetsnesfailures_ tsgetsnesfailures
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssetmaxsteprejections_ TSSETMAXSTEPREJECTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssetmaxsteprejections_ tssetmaxsteprejections
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssetmaxsnesfailures_ TSSETMAXSNESFAILURES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssetmaxsnesfailures_ tssetmaxsnesfailures
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsseterrorifstepfails_ TSSETERRORIFSTEPFAILS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsseterrorifstepfails_ tsseterrorifstepfails
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetadapt_ TSGETADAPT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetadapt_ tsgetadapt
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssettolerances_ TSSETTOLERANCES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssettolerances_ tssettolerances
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgettolerances_ TSGETTOLERANCES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgettolerances_ tsgettolerances
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tserrorweightednorm2_ TSERRORWEIGHTEDNORM2
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tserrorweightednorm2_ tserrorweightednorm2
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tserrorweightednorminfinity_ TSERRORWEIGHTEDNORMINFINITY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tserrorweightednorminfinity_ tserrorweightednorminfinity
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tserrorweightednorm_ TSERRORWEIGHTEDNORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tserrorweightednorm_ tserrorweightednorm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tserrorweightedenorm2_ TSERRORWEIGHTEDENORM2
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tserrorweightedenorm2_ tserrorweightedenorm2
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tserrorweightedenorminfinity_ TSERRORWEIGHTEDENORMINFINITY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tserrorweightedenorminfinity_ tserrorweightedenorminfinity
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tserrorweightedenorm_ TSERRORWEIGHTEDENORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tserrorweightedenorm_ tserrorweightedenorm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssetcfltimelocal_ TSSETCFLTIMELOCAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssetcfltimelocal_ tssetcfltimelocal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetcfltime_ TSGETCFLTIME
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetcfltime_ tsgetcfltime
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsvisetvariablebounds_ TSVISETVARIABLEBOUNDS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsvisetvariablebounds_ tsvisetvariablebounds
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tscomputelinearstability_ TSCOMPUTELINEARSTABILITY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tscomputelinearstability_ tscomputelinearstability
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsrestartstep_ TSRESTARTSTEP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsrestartstep_ tsrestartstep
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsrollback_ TSROLLBACK
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsrollback_ tsrollback
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetstages_ TSGETSTAGES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetstages_ tsgetstages
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssetfunctiondomainerror_ TSSETFUNCTIONDOMAINERROR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssetfunctiondomainerror_ tssetfunctiondomainerror
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsfunctiondomainerror_ TSFUNCTIONDOMAINERROR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsfunctiondomainerror_ tsfunctiondomainerror
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsrhsjacobiantest_ TSRHSJACOBIANTEST
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsrhsjacobiantest_ tsrhsjacobiantest
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  tssetfromoptions_(TS ts, int *__ierr){
*__ierr = TSSetFromOptions(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void PETSC_STDCALL  tsgettrajectory_(TS ts,TSTrajectory *tr, int *__ierr){
*__ierr = TSGetTrajectory(
	(TS)PetscToPointer((ts) ),tr);
}
PETSC_EXTERN void PETSC_STDCALL  tssetsavetrajectory_(TS ts, int *__ierr){
*__ierr = TSSetSaveTrajectory(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void PETSC_STDCALL  tscomputerhsjacobian_(TS ts,PetscReal *t,Vec U,Mat A,Mat B, int *__ierr){
*__ierr = TSComputeRHSJacobian(
	(TS)PetscToPointer((ts) ),*t,
	(Vec)PetscToPointer((U) ),
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ));
}
PETSC_EXTERN void PETSC_STDCALL  tscomputerhsfunction_(TS ts,PetscReal *t,Vec U,Vec y, int *__ierr){
*__ierr = TSComputeRHSFunction(
	(TS)PetscToPointer((ts) ),*t,
	(Vec)PetscToPointer((U) ),
	(Vec)PetscToPointer((y) ));
}
PETSC_EXTERN void PETSC_STDCALL  tscomputesolutionfunction_(TS ts,PetscReal *t,Vec U, int *__ierr){
*__ierr = TSComputeSolutionFunction(
	(TS)PetscToPointer((ts) ),*t,
	(Vec)PetscToPointer((U) ));
}
PETSC_EXTERN void PETSC_STDCALL  tscomputeforcingfunction_(TS ts,PetscReal *t,Vec U, int *__ierr){
*__ierr = TSComputeForcingFunction(
	(TS)PetscToPointer((ts) ),*t,
	(Vec)PetscToPointer((U) ));
}
PETSC_EXTERN void PETSC_STDCALL  tscomputeifunction_(TS ts,PetscReal *t,Vec U,Vec Udot,Vec Y,PetscBool *imex, int *__ierr){
*__ierr = TSComputeIFunction(
	(TS)PetscToPointer((ts) ),*t,
	(Vec)PetscToPointer((U) ),
	(Vec)PetscToPointer((Udot) ),
	(Vec)PetscToPointer((Y) ),*imex);
}
PETSC_EXTERN void PETSC_STDCALL  tscomputeijacobian_(TS ts,PetscReal *t,Vec U,Vec Udot,PetscReal *shift,Mat A,Mat B,PetscBool *imex, int *__ierr){
*__ierr = TSComputeIJacobian(
	(TS)PetscToPointer((ts) ),*t,
	(Vec)PetscToPointer((U) ),
	(Vec)PetscToPointer((Udot) ),*shift,
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ),*imex);
}
PETSC_EXTERN void PETSC_STDCALL  tsrhsjacobiansetreuse_(TS ts,PetscBool *reuse, int *__ierr){
*__ierr = TSRHSJacobianSetReuse(
	(TS)PetscToPointer((ts) ),*reuse);
}
PETSC_EXTERN void PETSC_STDCALL  tscomputei2function_(TS ts,PetscReal *t,Vec U,Vec V,Vec A,Vec F, int *__ierr){
*__ierr = TSComputeI2Function(
	(TS)PetscToPointer((ts) ),*t,
	(Vec)PetscToPointer((U) ),
	(Vec)PetscToPointer((V) ),
	(Vec)PetscToPointer((A) ),
	(Vec)PetscToPointer((F) ));
}
PETSC_EXTERN void PETSC_STDCALL  tscomputei2jacobian_(TS ts,PetscReal *t,Vec U,Vec V,Vec A,PetscReal *shiftV,PetscReal *shiftA,Mat J,Mat P, int *__ierr){
*__ierr = TSComputeI2Jacobian(
	(TS)PetscToPointer((ts) ),*t,
	(Vec)PetscToPointer((U) ),
	(Vec)PetscToPointer((V) ),
	(Vec)PetscToPointer((A) ),*shiftV,*shiftA,
	(Mat)PetscToPointer((J) ),
	(Mat)PetscToPointer((P) ));
}
PETSC_EXTERN void PETSC_STDCALL  ts2setsolution_(TS ts,Vec u,Vec v, int *__ierr){
*__ierr = TS2SetSolution(
	(TS)PetscToPointer((ts) ),
	(Vec)PetscToPointer((u) ),
	(Vec)PetscToPointer((v) ));
}
PETSC_EXTERN void PETSC_STDCALL  ts2getsolution_(TS ts,Vec *u,Vec *v, int *__ierr){
*__ierr = TS2GetSolution(
	(TS)PetscToPointer((ts) ),u,v);
}
PETSC_EXTERN void PETSC_STDCALL  tssetapplicationcontext_(TS ts,void*usrP, int *__ierr){
*__ierr = TSSetApplicationContext(
	(TS)PetscToPointer((ts) ),usrP);
}
PETSC_EXTERN void PETSC_STDCALL  tsgetapplicationcontext_(TS ts,void*usrP, int *__ierr){
*__ierr = TSGetApplicationContext(
	(TS)PetscToPointer((ts) ),usrP);
}
PETSC_EXTERN void PETSC_STDCALL  tsgetstepnumber_(TS ts,PetscInt *steps, int *__ierr){
*__ierr = TSGetStepNumber(
	(TS)PetscToPointer((ts) ),steps);
}
PETSC_EXTERN void PETSC_STDCALL  tssetstepnumber_(TS ts,PetscInt *steps, int *__ierr){
*__ierr = TSSetStepNumber(
	(TS)PetscToPointer((ts) ),*steps);
}
PETSC_EXTERN void PETSC_STDCALL  tssettimestep_(TS ts,PetscReal *time_step, int *__ierr){
*__ierr = TSSetTimeStep(
	(TS)PetscToPointer((ts) ),*time_step);
}
PETSC_EXTERN void PETSC_STDCALL  tssetexactfinaltime_(TS ts,TSExactFinalTimeOption *eftopt, int *__ierr){
*__ierr = TSSetExactFinalTime(
	(TS)PetscToPointer((ts) ),*eftopt);
}
PETSC_EXTERN void PETSC_STDCALL  tsgetexactfinaltime_(TS ts,TSExactFinalTimeOption *eftopt, int *__ierr){
*__ierr = TSGetExactFinalTime(
	(TS)PetscToPointer((ts) ),
	(TSExactFinalTimeOption* )PetscToPointer((eftopt) ));
}
PETSC_EXTERN void PETSC_STDCALL  tsgettimestep_(TS ts,PetscReal *dt, int *__ierr){
*__ierr = TSGetTimeStep(
	(TS)PetscToPointer((ts) ),dt);
}
PETSC_EXTERN void PETSC_STDCALL  tsgetsolution_(TS ts,Vec *v, int *__ierr){
*__ierr = TSGetSolution(
	(TS)PetscToPointer((ts) ),v);
}
PETSC_EXTERN void PETSC_STDCALL  tsgetsolutioncomponents_(TS ts,PetscInt *n,Vec *v, int *__ierr){
*__ierr = TSGetSolutionComponents(
	(TS)PetscToPointer((ts) ),n,v);
}
PETSC_EXTERN void PETSC_STDCALL  tsgetauxsolution_(TS ts,Vec *v, int *__ierr){
*__ierr = TSGetAuxSolution(
	(TS)PetscToPointer((ts) ),v);
}
PETSC_EXTERN void PETSC_STDCALL  tsgettimeerror_(TS ts,PetscInt *n,Vec *v, int *__ierr){
*__ierr = TSGetTimeError(
	(TS)PetscToPointer((ts) ),*n,v);
}
PETSC_EXTERN void PETSC_STDCALL  tssettimeerror_(TS ts,Vec v, int *__ierr){
*__ierr = TSSetTimeError(
	(TS)PetscToPointer((ts) ),
	(Vec)PetscToPointer((v) ));
}
PETSC_EXTERN void PETSC_STDCALL  tssetproblemtype_(TS ts,TSProblemType *type, int *__ierr){
*__ierr = TSSetProblemType(
	(TS)PetscToPointer((ts) ),*type);
}
PETSC_EXTERN void PETSC_STDCALL  tssetup_(TS ts, int *__ierr){
*__ierr = TSSetUp(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void PETSC_STDCALL  tsreset_(TS ts, int *__ierr){
*__ierr = TSReset(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void PETSC_STDCALL  tsdestroy_(TS *ts, int *__ierr){
*__ierr = TSDestroy(ts);
}
PETSC_EXTERN void PETSC_STDCALL  tsgetsnes_(TS ts,SNES *snes, int *__ierr){
*__ierr = TSGetSNES(
	(TS)PetscToPointer((ts) ),snes);
}
PETSC_EXTERN void PETSC_STDCALL  tssetsnes_(TS ts,SNES snes, int *__ierr){
*__ierr = TSSetSNES(
	(TS)PetscToPointer((ts) ),
	(SNES)PetscToPointer((snes) ));
}
PETSC_EXTERN void PETSC_STDCALL  tsgetksp_(TS ts,KSP *ksp, int *__ierr){
*__ierr = TSGetKSP(
	(TS)PetscToPointer((ts) ),ksp);
}
PETSC_EXTERN void PETSC_STDCALL  tssetmaxsteps_(TS ts,PetscInt *maxsteps, int *__ierr){
*__ierr = TSSetMaxSteps(
	(TS)PetscToPointer((ts) ),*maxsteps);
}
PETSC_EXTERN void PETSC_STDCALL  tsgetmaxsteps_(TS ts,PetscInt *maxsteps, int *__ierr){
*__ierr = TSGetMaxSteps(
	(TS)PetscToPointer((ts) ),maxsteps);
}
PETSC_EXTERN void PETSC_STDCALL  tssetmaxtime_(TS ts,PetscReal *maxtime, int *__ierr){
*__ierr = TSSetMaxTime(
	(TS)PetscToPointer((ts) ),*maxtime);
}
PETSC_EXTERN void PETSC_STDCALL  tsgetmaxtime_(TS ts,PetscReal *maxtime, int *__ierr){
*__ierr = TSGetMaxTime(
	(TS)PetscToPointer((ts) ),maxtime);
}
PETSC_EXTERN void PETSC_STDCALL  tssetinitialtimestep_(TS ts,PetscReal *initial_time,PetscReal *time_step, int *__ierr){
*__ierr = TSSetInitialTimeStep(
	(TS)PetscToPointer((ts) ),*initial_time,*time_step);
}
PETSC_EXTERN void PETSC_STDCALL  tsgetduration_(TS ts,PetscInt *maxsteps,PetscReal *maxtime, int *__ierr){
*__ierr = TSGetDuration(
	(TS)PetscToPointer((ts) ),maxsteps,maxtime);
}
PETSC_EXTERN void PETSC_STDCALL  tssetduration_(TS ts,PetscInt *maxsteps,PetscReal *maxtime, int *__ierr){
*__ierr = TSSetDuration(
	(TS)PetscToPointer((ts) ),*maxsteps,*maxtime);
}
PETSC_EXTERN void PETSC_STDCALL  tsgettimestepnumber_(TS ts,PetscInt *steps, int *__ierr){
*__ierr = TSGetTimeStepNumber(
	(TS)PetscToPointer((ts) ),steps);
}
PETSC_EXTERN void PETSC_STDCALL  tsgettotalsteps_(TS ts,PetscInt *steps, int *__ierr){
*__ierr = TSGetTotalSteps(
	(TS)PetscToPointer((ts) ),steps);
}
PETSC_EXTERN void PETSC_STDCALL  tssetsolution_(TS ts,Vec u, int *__ierr){
*__ierr = TSSetSolution(
	(TS)PetscToPointer((ts) ),
	(Vec)PetscToPointer((u) ));
}
PETSC_EXTERN void PETSC_STDCALL  tsprestep_(TS ts, int *__ierr){
*__ierr = TSPreStep(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void PETSC_STDCALL  tsprestage_(TS ts,PetscReal *stagetime, int *__ierr){
*__ierr = TSPreStage(
	(TS)PetscToPointer((ts) ),*stagetime);
}
PETSC_EXTERN void PETSC_STDCALL  tspoststage_(TS ts,PetscReal *stagetime,PetscInt *stageindex,Vec *Y, int *__ierr){
*__ierr = TSPostStage(
	(TS)PetscToPointer((ts) ),*stagetime,*stageindex,Y);
}
PETSC_EXTERN void PETSC_STDCALL  tspostevaluate_(TS ts, int *__ierr){
*__ierr = TSPostEvaluate(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void PETSC_STDCALL  tspoststep_(TS ts, int *__ierr){
*__ierr = TSPostStep(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void PETSC_STDCALL  tsinterpolate_(TS ts,PetscReal *t,Vec U, int *__ierr){
*__ierr = TSInterpolate(
	(TS)PetscToPointer((ts) ),*t,
	(Vec)PetscToPointer((U) ));
}
PETSC_EXTERN void PETSC_STDCALL  tsstep_(TS ts, int *__ierr){
*__ierr = TSStep(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void PETSC_STDCALL  tsevaluatewlte_(TS ts,NormType *wnormtype,PetscInt *order,PetscReal *wlte, int *__ierr){
*__ierr = TSEvaluateWLTE(
	(TS)PetscToPointer((ts) ),*wnormtype,order,wlte);
}
PETSC_EXTERN void PETSC_STDCALL  tsevaluatestep_(TS ts,PetscInt *order,Vec U,PetscBool *done, int *__ierr){
*__ierr = TSEvaluateStep(
	(TS)PetscToPointer((ts) ),*order,
	(Vec)PetscToPointer((U) ),done);
}
PETSC_EXTERN void PETSC_STDCALL  tssolve_(TS ts,Vec u, int *__ierr){
*__ierr = TSSolve(
	(TS)PetscToPointer((ts) ),
	(Vec)PetscToPointer((u) ));
}
PETSC_EXTERN void PETSC_STDCALL  tsgettime_(TS ts,PetscReal *t, int *__ierr){
*__ierr = TSGetTime(
	(TS)PetscToPointer((ts) ),t);
}
PETSC_EXTERN void PETSC_STDCALL  tsgetprevtime_(TS ts,PetscReal *t, int *__ierr){
*__ierr = TSGetPrevTime(
	(TS)PetscToPointer((ts) ),t);
}
PETSC_EXTERN void PETSC_STDCALL  tssettime_(TS ts,PetscReal *t, int *__ierr){
*__ierr = TSSetTime(
	(TS)PetscToPointer((ts) ),*t);
}
PETSC_EXTERN void PETSC_STDCALL  tssetdm_(TS ts,DM dm, int *__ierr){
*__ierr = TSSetDM(
	(TS)PetscToPointer((ts) ),
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void PETSC_STDCALL  tsgetdm_(TS ts,DM *dm, int *__ierr){
*__ierr = TSGetDM(
	(TS)PetscToPointer((ts) ),dm);
}
PETSC_EXTERN void PETSC_STDCALL  snestsformfunction_(SNES snes,Vec U,Vec F,void*ctx, int *__ierr){
*__ierr = SNESTSFormFunction(
	(SNES)PetscToPointer((snes) ),
	(Vec)PetscToPointer((U) ),
	(Vec)PetscToPointer((F) ),ctx);
}
PETSC_EXTERN void PETSC_STDCALL  snestsformjacobian_(SNES snes,Vec U,Mat A,Mat B,void*ctx, int *__ierr){
*__ierr = SNESTSFormJacobian(
	(SNES)PetscToPointer((snes) ),
	(Vec)PetscToPointer((U) ),
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ),ctx);
}
PETSC_EXTERN void PETSC_STDCALL  tsgetequationtype_(TS ts,TSEquationType *equation_type, int *__ierr){
*__ierr = TSGetEquationType(
	(TS)PetscToPointer((ts) ),
	(TSEquationType* )PetscToPointer((equation_type) ));
}
PETSC_EXTERN void PETSC_STDCALL  tssetequationtype_(TS ts,TSEquationType *equation_type, int *__ierr){
*__ierr = TSSetEquationType(
	(TS)PetscToPointer((ts) ),*equation_type);
}
PETSC_EXTERN void PETSC_STDCALL  tsgetconvergedreason_(TS ts,TSConvergedReason *reason, int *__ierr){
*__ierr = TSGetConvergedReason(
	(TS)PetscToPointer((ts) ),reason);
}
PETSC_EXTERN void PETSC_STDCALL  tssetconvergedreason_(TS ts,TSConvergedReason *reason, int *__ierr){
*__ierr = TSSetConvergedReason(
	(TS)PetscToPointer((ts) ),*reason);
}
PETSC_EXTERN void PETSC_STDCALL  tsgetsolvetime_(TS ts,PetscReal *ftime, int *__ierr){
*__ierr = TSGetSolveTime(
	(TS)PetscToPointer((ts) ),ftime);
}
PETSC_EXTERN void PETSC_STDCALL  tsgetsnesiterations_(TS ts,PetscInt *nits, int *__ierr){
*__ierr = TSGetSNESIterations(
	(TS)PetscToPointer((ts) ),nits);
}
PETSC_EXTERN void PETSC_STDCALL  tsgetkspiterations_(TS ts,PetscInt *lits, int *__ierr){
*__ierr = TSGetKSPIterations(
	(TS)PetscToPointer((ts) ),lits);
}
PETSC_EXTERN void PETSC_STDCALL  tsgetsteprejections_(TS ts,PetscInt *rejects, int *__ierr){
*__ierr = TSGetStepRejections(
	(TS)PetscToPointer((ts) ),rejects);
}
PETSC_EXTERN void PETSC_STDCALL  tsgetsnesfailures_(TS ts,PetscInt *fails, int *__ierr){
*__ierr = TSGetSNESFailures(
	(TS)PetscToPointer((ts) ),fails);
}
PETSC_EXTERN void PETSC_STDCALL  tssetmaxsteprejections_(TS ts,PetscInt *rejects, int *__ierr){
*__ierr = TSSetMaxStepRejections(
	(TS)PetscToPointer((ts) ),*rejects);
}
PETSC_EXTERN void PETSC_STDCALL  tssetmaxsnesfailures_(TS ts,PetscInt *fails, int *__ierr){
*__ierr = TSSetMaxSNESFailures(
	(TS)PetscToPointer((ts) ),*fails);
}
PETSC_EXTERN void PETSC_STDCALL  tsseterrorifstepfails_(TS ts,PetscBool *err, int *__ierr){
*__ierr = TSSetErrorIfStepFails(
	(TS)PetscToPointer((ts) ),*err);
}
PETSC_EXTERN void PETSC_STDCALL  tsgetadapt_(TS ts,TSAdapt *adapt, int *__ierr){
*__ierr = TSGetAdapt(
	(TS)PetscToPointer((ts) ),adapt);
}
PETSC_EXTERN void PETSC_STDCALL  tssettolerances_(TS ts,PetscReal *atol,Vec vatol,PetscReal *rtol,Vec vrtol, int *__ierr){
*__ierr = TSSetTolerances(
	(TS)PetscToPointer((ts) ),*atol,
	(Vec)PetscToPointer((vatol) ),*rtol,
	(Vec)PetscToPointer((vrtol) ));
}
PETSC_EXTERN void PETSC_STDCALL  tsgettolerances_(TS ts,PetscReal *atol,Vec *vatol,PetscReal *rtol,Vec *vrtol, int *__ierr){
*__ierr = TSGetTolerances(
	(TS)PetscToPointer((ts) ),atol,vatol,rtol,vrtol);
}
PETSC_EXTERN void PETSC_STDCALL  tserrorweightednorm2_(TS ts,Vec U,Vec Y,PetscReal *norm,PetscReal *norma,PetscReal *normr, int *__ierr){
*__ierr = TSErrorWeightedNorm2(
	(TS)PetscToPointer((ts) ),
	(Vec)PetscToPointer((U) ),
	(Vec)PetscToPointer((Y) ),norm,norma,normr);
}
PETSC_EXTERN void PETSC_STDCALL  tserrorweightednorminfinity_(TS ts,Vec U,Vec Y,PetscReal *norm,PetscReal *norma,PetscReal *normr, int *__ierr){
*__ierr = TSErrorWeightedNormInfinity(
	(TS)PetscToPointer((ts) ),
	(Vec)PetscToPointer((U) ),
	(Vec)PetscToPointer((Y) ),norm,norma,normr);
}
PETSC_EXTERN void PETSC_STDCALL  tserrorweightednorm_(TS ts,Vec U,Vec Y,NormType *wnormtype,PetscReal *norm,PetscReal *norma,PetscReal *normr, int *__ierr){
*__ierr = TSErrorWeightedNorm(
	(TS)PetscToPointer((ts) ),
	(Vec)PetscToPointer((U) ),
	(Vec)PetscToPointer((Y) ),*wnormtype,norm,norma,normr);
}
PETSC_EXTERN void PETSC_STDCALL  tserrorweightedenorm2_(TS ts,Vec E,Vec U,Vec Y,PetscReal *norm,PetscReal *norma,PetscReal *normr, int *__ierr){
*__ierr = TSErrorWeightedENorm2(
	(TS)PetscToPointer((ts) ),
	(Vec)PetscToPointer((E) ),
	(Vec)PetscToPointer((U) ),
	(Vec)PetscToPointer((Y) ),norm,norma,normr);
}
PETSC_EXTERN void PETSC_STDCALL  tserrorweightedenorminfinity_(TS ts,Vec E,Vec U,Vec Y,PetscReal *norm,PetscReal *norma,PetscReal *normr, int *__ierr){
*__ierr = TSErrorWeightedENormInfinity(
	(TS)PetscToPointer((ts) ),
	(Vec)PetscToPointer((E) ),
	(Vec)PetscToPointer((U) ),
	(Vec)PetscToPointer((Y) ),norm,norma,normr);
}
PETSC_EXTERN void PETSC_STDCALL  tserrorweightedenorm_(TS ts,Vec E,Vec U,Vec Y,NormType *wnormtype,PetscReal *norm,PetscReal *norma,PetscReal *normr, int *__ierr){
*__ierr = TSErrorWeightedENorm(
	(TS)PetscToPointer((ts) ),
	(Vec)PetscToPointer((E) ),
	(Vec)PetscToPointer((U) ),
	(Vec)PetscToPointer((Y) ),*wnormtype,norm,norma,normr);
}
PETSC_EXTERN void PETSC_STDCALL  tssetcfltimelocal_(TS ts,PetscReal *cfltime, int *__ierr){
*__ierr = TSSetCFLTimeLocal(
	(TS)PetscToPointer((ts) ),*cfltime);
}
PETSC_EXTERN void PETSC_STDCALL  tsgetcfltime_(TS ts,PetscReal *cfltime, int *__ierr){
*__ierr = TSGetCFLTime(
	(TS)PetscToPointer((ts) ),cfltime);
}
PETSC_EXTERN void PETSC_STDCALL  tsvisetvariablebounds_(TS ts,Vec xl,Vec xu, int *__ierr){
*__ierr = TSVISetVariableBounds(
	(TS)PetscToPointer((ts) ),
	(Vec)PetscToPointer((xl) ),
	(Vec)PetscToPointer((xu) ));
}
PETSC_EXTERN void PETSC_STDCALL  tscomputelinearstability_(TS ts,PetscReal *xr,PetscReal *xi,PetscReal *yr,PetscReal *yi, int *__ierr){
*__ierr = TSComputeLinearStability(
	(TS)PetscToPointer((ts) ),*xr,*xi,yr,yi);
}
PETSC_EXTERN void PETSC_STDCALL  tsrestartstep_(TS ts, int *__ierr){
*__ierr = TSRestartStep(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void PETSC_STDCALL  tsrollback_(TS ts, int *__ierr){
*__ierr = TSRollBack(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void PETSC_STDCALL  tsgetstages_(TS ts,PetscInt *ns,Vec **Y, int *__ierr){
*__ierr = TSGetStages(
	(TS)PetscToPointer((ts) ),ns,Y);
}

PETSC_EXTERN void PETSC_STDCALL  tssetfunctiondomainerror_(TS ts,PetscErrorCode (*func)(TS,PetscReal,Vec,PetscBool*), int *__ierr){
*__ierr = TSSetFunctionDomainError(
	(TS)PetscToPointer((ts) ),func);
}
PETSC_EXTERN void PETSC_STDCALL  tsfunctiondomainerror_(TS ts,PetscReal *stagetime,Vec Y,PetscBool* accept, int *__ierr){
*__ierr = TSFunctionDomainError(
	(TS)PetscToPointer((ts) ),*stagetime,
	(Vec)PetscToPointer((Y) ),accept);
}
PETSC_EXTERN void PETSC_STDCALL  tsrhsjacobiantest_(TS ts,PetscBool *flg, int *__ierr){
*__ierr = TSRHSJacobianTest(
	(TS)PetscToPointer((ts) ),flg);
}
#if defined(__cplusplus)
}
#endif
