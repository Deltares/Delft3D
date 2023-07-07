#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dscatter.c */
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

#include "petscdraw.h"
#include "petscsys.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawspsetdimension_ PETSCDRAWSPSETDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawspsetdimension_ petscdrawspsetdimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawspreset_ PETSCDRAWSPRESET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawspreset_ petscdrawspreset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawspaddpoint_ PETSCDRAWSPADDPOINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawspaddpoint_ petscdrawspaddpoint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawspdraw_ PETSCDRAWSPDRAW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawspdraw_ petscdrawspdraw
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawspsave_ PETSCDRAWSPSAVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawspsave_ petscdrawspsave
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawspsetlimits_ PETSCDRAWSPSETLIMITS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawspsetlimits_ petscdrawspsetlimits
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  petscdrawspsetdimension_(PetscDrawSP sp,int *dim, int *__ierr){
*__ierr = PetscDrawSPSetDimension(
	(PetscDrawSP)PetscToPointer((sp) ),*dim);
}
PETSC_EXTERN void PETSC_STDCALL  petscdrawspreset_(PetscDrawSP sp, int *__ierr){
*__ierr = PetscDrawSPReset(
	(PetscDrawSP)PetscToPointer((sp) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscdrawspaddpoint_(PetscDrawSP sp,PetscReal *x,PetscReal *y, int *__ierr){
*__ierr = PetscDrawSPAddPoint(
	(PetscDrawSP)PetscToPointer((sp) ),x,y);
}
PETSC_EXTERN void PETSC_STDCALL  petscdrawspdraw_(PetscDrawSP sp,PetscBool *clear, int *__ierr){
*__ierr = PetscDrawSPDraw(
	(PetscDrawSP)PetscToPointer((sp) ),*clear);
}
PETSC_EXTERN void PETSC_STDCALL  petscdrawspsave_(PetscDrawSP sp, int *__ierr){
*__ierr = PetscDrawSPSave(
	(PetscDrawSP)PetscToPointer((sp) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscdrawspsetlimits_(PetscDrawSP sp,PetscReal *x_min,PetscReal *x_max,PetscReal *y_min,PetscReal *y_max, int *__ierr){
*__ierr = PetscDrawSPSetLimits(
	(PetscDrawSP)PetscToPointer((sp) ),*x_min,*x_max,*y_min,*y_max);
}
#if defined(__cplusplus)
}
#endif
