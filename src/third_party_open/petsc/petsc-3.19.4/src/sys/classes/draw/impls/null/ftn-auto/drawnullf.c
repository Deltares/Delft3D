#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* drawnull.c */
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
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawopennull_ PETSCDRAWOPENNULL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawopennull_ petscdrawopennull
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawisnull_ PETSCDRAWISNULL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawisnull_ petscdrawisnull
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscdrawopennull_(MPI_Fint * comm,PetscDraw *win, int *__ierr)
{
*__ierr = PetscDrawOpenNull(
	MPI_Comm_f2c(*(comm)),win);
}
PETSC_EXTERN void  petscdrawisnull_(PetscDraw draw,PetscBool *yes, int *__ierr)
{
*__ierr = PetscDrawIsNull(
	(PetscDraw)PetscToPointer((draw) ),yes);
}
#if defined(__cplusplus)
}
#endif
