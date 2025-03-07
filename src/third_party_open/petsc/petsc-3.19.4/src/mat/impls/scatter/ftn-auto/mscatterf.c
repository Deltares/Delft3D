#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* mscatter.c */
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

#include "petscmat.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matscattergetvecscatter_ MATSCATTERGETVECSCATTER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matscattergetvecscatter_ matscattergetvecscatter
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matscattersetvecscatter_ MATSCATTERSETVECSCATTER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matscattersetvecscatter_ matscattersetvecscatter
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matscattergetvecscatter_(Mat mat,VecScatter *scatter, int *__ierr)
{
*__ierr = MatScatterGetVecScatter(
	(Mat)PetscToPointer((mat) ),scatter);
}
PETSC_EXTERN void  matscattersetvecscatter_(Mat mat,VecScatter scatter, int *__ierr)
{
*__ierr = MatScatterSetVecScatter(
	(Mat)PetscToPointer((mat) ),
	(VecScatter)PetscToPointer((scatter) ));
}
#if defined(__cplusplus)
}
#endif
