#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* bdf.c */
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
#define tsbdfsetorder_ TSBDFSETORDER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsbdfsetorder_ tsbdfsetorder
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsbdfgetorder_ TSBDFGETORDER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsbdfgetorder_ tsbdfgetorder
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  tsbdfsetorder_(TS ts,PetscInt *order, int *__ierr)
{
*__ierr = TSBDFSetOrder(
	(TS)PetscToPointer((ts) ),*order);
}
PETSC_EXTERN void  tsbdfgetorder_(TS ts,PetscInt *order, int *__ierr)
{
*__ierr = TSBDFGetOrder(
	(TS)PetscToPointer((ts) ),order);
}
#if defined(__cplusplus)
}
#endif
