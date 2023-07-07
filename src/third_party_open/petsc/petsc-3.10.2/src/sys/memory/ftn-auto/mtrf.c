#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* mtr.c */
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

#include "petscsys.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscmallocgetcurrentusage_ PETSCMALLOCGETCURRENTUSAGE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscmallocgetcurrentusage_ petscmallocgetcurrentusage
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscmallocgetmaximumusage_ PETSCMALLOCGETMAXIMUMUSAGE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscmallocgetmaximumusage_ petscmallocgetmaximumusage
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscmallocsetdumplog_ PETSCMALLOCSETDUMPLOG
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscmallocsetdumplog_ petscmallocsetdumplog
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscmallocsetdumplogthreshold_ PETSCMALLOCSETDUMPLOGTHRESHOLD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscmallocsetdumplogthreshold_ petscmallocsetdumplogthreshold
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscmallocgetdumplog_ PETSCMALLOCGETDUMPLOG
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscmallocgetdumplog_ petscmallocgetdumplog
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscmallocdebug_ PETSCMALLOCDEBUG
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscmallocdebug_ petscmallocdebug
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscmallocgetdebug_ PETSCMALLOCGETDEBUG
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscmallocgetdebug_ petscmallocgetdebug
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  petscmallocgetcurrentusage_(PetscLogDouble *space, int *__ierr){
*__ierr = PetscMallocGetCurrentUsage(space);
}
PETSC_EXTERN void PETSC_STDCALL  petscmallocgetmaximumusage_(PetscLogDouble *space, int *__ierr){
*__ierr = PetscMallocGetMaximumUsage(space);
}
PETSC_EXTERN void PETSC_STDCALL  petscmallocsetdumplog_(int *__ierr ){
*__ierr = PetscMallocSetDumpLog();
}
PETSC_EXTERN void PETSC_STDCALL  petscmallocsetdumplogthreshold_(PetscLogDouble *logmin, int *__ierr){
*__ierr = PetscMallocSetDumpLogThreshold(*logmin);
}
PETSC_EXTERN void PETSC_STDCALL  petscmallocgetdumplog_(PetscBool *logging, int *__ierr){
*__ierr = PetscMallocGetDumpLog(logging);
}
PETSC_EXTERN void PETSC_STDCALL  petscmallocdebug_(PetscBool *level, int *__ierr){
*__ierr = PetscMallocDebug(*level);
}
PETSC_EXTERN void PETSC_STDCALL  petscmallocgetdebug_(PetscBool *flg, int *__ierr){
*__ierr = PetscMallocGetDebug(flg);
}
#if defined(__cplusplus)
}
#endif
