#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* iguess.c */
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

#include "petscksp.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspguesssetfromoptions_ KSPGUESSSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspguesssetfromoptions_ kspguesssetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspguessdestroy_ KSPGUESSDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspguessdestroy_ kspguessdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspguesscreate_ KSPGUESSCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspguesscreate_ kspguesscreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspguessupdate_ KSPGUESSUPDATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspguessupdate_ kspguessupdate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspguessformguess_ KSPGUESSFORMGUESS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspguessformguess_ kspguessformguess
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspguesssetup_ KSPGUESSSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspguesssetup_ kspguesssetup
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  kspguesssetfromoptions_(KSPGuess *guess, int *__ierr){
*__ierr = KSPGuessSetFromOptions(*guess);
}
PETSC_EXTERN void PETSC_STDCALL  kspguessdestroy_(KSPGuess *guess, int *__ierr){
*__ierr = KSPGuessDestroy(
	(KSPGuess* )PetscToPointer((guess) ));
}
PETSC_EXTERN void PETSC_STDCALL  kspguesscreate_(MPI_Fint * comm,KSPGuess *guess, int *__ierr){
*__ierr = KSPGuessCreate(
	MPI_Comm_f2c(*(comm)),
	(KSPGuess* )PetscToPointer((guess) ));
}
PETSC_EXTERN void PETSC_STDCALL  kspguessupdate_(KSPGuess *guess,Vec rhs,Vec sol, int *__ierr){
*__ierr = KSPGuessUpdate(*guess,
	(Vec)PetscToPointer((rhs) ),
	(Vec)PetscToPointer((sol) ));
}
PETSC_EXTERN void PETSC_STDCALL  kspguessformguess_(KSPGuess *guess,Vec rhs,Vec sol, int *__ierr){
*__ierr = KSPGuessFormGuess(*guess,
	(Vec)PetscToPointer((rhs) ),
	(Vec)PetscToPointer((sol) ));
}
PETSC_EXTERN void PETSC_STDCALL  kspguesssetup_(KSPGuess *guess, int *__ierr){
*__ierr = KSPGuessSetUp(*guess);
}
#if defined(__cplusplus)
}
#endif
