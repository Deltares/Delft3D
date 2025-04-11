#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* symbrdn.c */
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
#define matlmvmsymbroydensetdelta_ MATLMVMSYMBROYDENSETDELTA
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matlmvmsymbroydensetdelta_ matlmvmsymbroydensetdelta
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matlmvmsymbroydensetscaletype_ MATLMVMSYMBROYDENSETSCALETYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matlmvmsymbroydensetscaletype_ matlmvmsymbroydensetscaletype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcreatelmvmsymbroyden_ MATCREATELMVMSYMBROYDEN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreatelmvmsymbroyden_ matcreatelmvmsymbroyden
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif

PETSC_EXTERN void  matlmvmsymbroydensetdelta_(Mat B,PetscScalar *delta, int *__ierr)
{
*__ierr = MatLMVMSymBroydenSetDelta(
	(Mat)PetscToPointer((B) ),*delta);
}
PETSC_EXTERN void  matlmvmsymbroydensetscaletype_(Mat B,MatLMVMSymBroydenScaleType *stype, int *__ierr)
{
*__ierr = MatLMVMSymBroydenSetScaleType(
	(Mat)PetscToPointer((B) ),*stype);
}
PETSC_EXTERN void  matcreatelmvmsymbroyden_(MPI_Fint * comm,PetscInt *n,PetscInt *N,Mat *B, int *__ierr)
{
*__ierr = MatCreateLMVMSymBroyden(
	MPI_Comm_f2c(*(comm)),*n,*N,B);
}
#if defined(__cplusplus)
}
#endif
