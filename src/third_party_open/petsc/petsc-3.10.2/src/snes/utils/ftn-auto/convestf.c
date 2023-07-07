#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* convest.c */
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

#include "petscconvest.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscconvestcreate_ PETSCCONVESTCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscconvestcreate_ petscconvestcreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscconvestdestroy_ PETSCCONVESTDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscconvestdestroy_ petscconvestdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscconvestsetfromoptions_ PETSCCONVESTSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscconvestsetfromoptions_ petscconvestsetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscconvestview_ PETSCCONVESTVIEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscconvestview_ petscconvestview
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscconvestgetsolver_ PETSCCONVESTGETSOLVER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscconvestgetsolver_ petscconvestgetsolver
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscconvestsetsolver_ PETSCCONVESTSETSOLVER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscconvestsetsolver_ petscconvestsetsolver
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscconvestsetup_ PETSCCONVESTSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscconvestsetup_ petscconvestsetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscconvestgetconvrate_ PETSCCONVESTGETCONVRATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscconvestgetconvrate_ petscconvestgetconvrate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscconvestrateview_ PETSCCONVESTRATEVIEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscconvestrateview_ petscconvestrateview
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  petscconvestcreate_(MPI_Fint * comm,PetscConvEst *ce, int *__ierr){
*__ierr = PetscConvEstCreate(
	MPI_Comm_f2c(*(comm)),
	(PetscConvEst* )PetscToPointer((ce) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscconvestdestroy_(PetscConvEst *ce, int *__ierr){
*__ierr = PetscConvEstDestroy(
	(PetscConvEst* )PetscToPointer((ce) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscconvestsetfromoptions_(PetscConvEst *ce, int *__ierr){
*__ierr = PetscConvEstSetFromOptions(*ce);
}
PETSC_EXTERN void PETSC_STDCALL  petscconvestview_(PetscConvEst *ce,PetscViewer viewer, int *__ierr){
*__ierr = PetscConvEstView(*ce,
	(PetscViewer)PetscToPointer((viewer) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscconvestgetsolver_(PetscConvEst *ce,SNES *snes, int *__ierr){
*__ierr = PetscConvEstGetSolver(*ce,snes);
}
PETSC_EXTERN void PETSC_STDCALL  petscconvestsetsolver_(PetscConvEst *ce,SNES snes, int *__ierr){
*__ierr = PetscConvEstSetSolver(*ce,
	(SNES)PetscToPointer((snes) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscconvestsetup_(PetscConvEst *ce, int *__ierr){
*__ierr = PetscConvEstSetUp(*ce);
}
PETSC_EXTERN void PETSC_STDCALL  petscconvestgetconvrate_(PetscConvEst *ce,PetscReal alpha[], int *__ierr){
*__ierr = PetscConvEstGetConvRate(*ce,alpha);
}
PETSC_EXTERN void PETSC_STDCALL  petscconvestrateview_(PetscConvEst *ce,PetscReal alpha[],PetscViewer viewer, int *__ierr){
*__ierr = PetscConvEstRateView(*ce,alpha,
	(PetscViewer)PetscToPointer((viewer) ));
}
#if defined(__cplusplus)
}
#endif
