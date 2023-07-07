#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* pf.c */
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

#include "petscpf.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pfapplyvec_ PFAPPLYVEC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pfapplyvec_ pfapplyvec
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pfapply_ PFAPPLY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pfapply_ pfapply
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pfview_ PFVIEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pfview_ pfview
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pfsetfromoptions_ PFSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pfsetfromoptions_ pfsetfromoptions
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  pfapplyvec_(PF pf,Vec x,Vec y, int *__ierr){
*__ierr = PFApplyVec(
	(PF)PetscToPointer((pf) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ));
}
PETSC_EXTERN void PETSC_STDCALL  pfapply_(PF pf,PetscInt *n, PetscScalar *x,PetscScalar *y, int *__ierr){
*__ierr = PFApply(
	(PF)PetscToPointer((pf) ),*n,x,y);
}
PETSC_EXTERN void PETSC_STDCALL  pfview_(PF pf,PetscViewer viewer, int *__ierr){
*__ierr = PFView(
	(PF)PetscToPointer((pf) ),
	(PetscViewer)PetscToPointer((viewer) ));
}
PETSC_EXTERN void PETSC_STDCALL  pfsetfromoptions_(PF pf, int *__ierr){
*__ierr = PFSetFromOptions(
	(PF)PetscToPointer((pf) ));
}
#if defined(__cplusplus)
}
#endif
